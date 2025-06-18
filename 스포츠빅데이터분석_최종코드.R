setwd("/Users/imyerim/Desktop")

library(baseballr); library(purrr);     library(tidyr)
library(httr);     library(jsonlite);   library(progress)
library(ggplot2);  library(ggrepel);    library(tibble)
library(dplyr);    library(lubridate);  library(showtext)
library(sysfonts); library(readr);      library(janitor)
library(pscl);     library(tidyverse);  library(slider)
library(pROC);     library(caret)

font_add_google(name = "Nanum Gothic", family = "nanum")
showtext_auto()            
theme_set(theme_minimal(base_family = "nanum"))

get_game_pks <- function(season){
  mlb_schedule(season = season, level_ids = 1) %>%     
    filter(game_type == "R") %>%                       
    select(game_pk, game_date = game_date, 
           home_id  = teams_home_team_id,
           away_id  = teams_away_team_id)              
}

pks <- bind_rows(get_game_pks(2024), get_game_pks(2025))

get_runs <- function(pk) {
  url <- paste0("https://statsapi.mlb.com/api/v1/game/", pk, "/linescore")
  res <- GET(url, timeout(10))
  if (http_error(res)) return(c(NA, NA))
  
  js <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  
  c(js$teams$home$runs, js$teams$away$runs)
}

parse_box <- function(pk, date){
  url  <- paste0("https://statsapi.mlb.com/api/v1/game/", pk, "/boxscore")
  res  <- GET(url, timeout(10))
  if (http_error(res)) return(NULL)
  
  js   <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  runs <- get_runs(pk) 
  
  tibble(
    game_pk   = pk,
    game_date = date,
    team_id   = c(js$teams$home$team$id,
                  js$teams$away$team$id),
    team_name = c(js$teams$home$team$name,
                  js$teams$away$team$name),
    runs      = runs,  
    bb        = c(js$teams$home$teamStats$pitching$baseOnBalls,
                  js$teams$away$teamStats$pitching$baseOnBalls),
    so        = c(js$teams$home$teamStats$pitching$strikeOuts,
                  js$teams$away$teamStats$pitching$strikeOuts)
  ) %>%
    mutate(k_bb = ifelse(bb == 0, NA_real_, so / bb))
}

pb <- progress_bar$new(total = nrow(pks), format = "[:bar] :current/:total :eta")

all_games <- pmap_dfr(
  list(pks$game_pk, pks$game_date), 
  ~{ pb$tick(); parse_box(..1, ..2) }, 
  .id = NULL
)

data <- all_games %>%
  mutate(
    year = as.integer(substr(game_date, 1,4)),
    month = as.integer(substr(game_date, 6,7)),
    day = as.integer(substr(game_date, 9,10)),
    team_name = ifelse(team_name == "Athletics", "Oakland Athletics", team_name )
  ) %>%
  select(game_pk, game_date, year, month, day, team_id, team_name, runs, bb, so, k_bb)

data <- data %>% 
  mutate(game_date = as.Date(game_date)) %>% 
  arrange(game_date, game_pk, team_id) %>% 
  filter(game_date <= as.Date("2025-06-01"))

na_rows <- which(is.na(data$runs))
true_runs <- c(3, 4, 0, 4, NA, NA, 5, 6, 5, 3)
data$runs[na_rows] <- true_runs
data <- data %>% filter(!is.na(runs))

data <- data %>%
  arrange(team_id, game_date) %>%  
  group_by(team_id) %>%
  fill(k_bb, .direction = "down") %>%  
  ungroup()

data <- data %>%
  mutate(
    k_bb = ifelse(is.na(k_bb), 0, k_bb)
  )

data_cum <- data %>% 
  group_by(team_id) %>% 
  arrange(game_date, .by_group = TRUE) %>% 
  mutate(
    games_played     = row_number(),
    cum_runs_sum     = cumsum(runs),
    cum_k_bb_sum     = cumsum(k_bb),
    cum_runs_mean    = cum_runs_sum / games_played,
    cum_k_bb_mean    = cum_k_bb_sum / games_played
  ) %>% 
  ungroup()

data_out <- data_cum %>% 
  group_by(game_date) %>% 
  mutate(
    league_runs_mean = mean(cum_runs_mean,  na.rm = TRUE),
    league_runs_sd   = sd  (cum_runs_mean,  na.rm = TRUE),
    league_k_bb_mean = mean(cum_k_bb_mean,  na.rm = TRUE),
    league_k_bb_sd   = sd  (cum_k_bb_mean,  na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    outlier_runs_z =(cum_runs_mean - league_runs_mean) / league_runs_sd,
    outlier_bbk_z  = (cum_k_bb_mean - league_k_bb_mean) / league_k_bb_sd
  )

final_data <-  data_out %>% mutate(
  shifted_outlier_runs_z = outlier_runs_z + 4,
  shifted_outlier_bbk_z = outlier_bbk_z + 4, 
  outlier_score = sqrt(shifted_outlier_runs_z^2 + shifted_outlier_bbk_z^2) * 50
)

predict_data <- final_data %>%
  filter(game_date >= as.Date("2025-05-23") & game_date <= as.Date("2025-06-01"))

##############################
######### ELO 구하기 ######### 
##############################
# ─ 1. 원본 데이터 불러오기 ─
raw <- read_csv("24-25_MLB_without ELO.csv", show_col_types = FALSE)

# ─ 2. 투구 단위 → 경기 단위 요약 ─
games <- raw %>% 
  group_by(game_pk) %>% 
  summarise(
    game_date = as_date(first(game_date_x)),
    home_team = first(home_team),
    away_team = first(away_team),
    home_runs = first(home_runs),
    away_runs = first(away_runs),
    .groups = "drop"
  ) %>% 
  filter(!is.na(home_runs) & !is.na(away_runs)) %>% 
  mutate(season = year(game_date))

# ─ 3. Elo 계산 함수(경기 전 Elo 모두 기록) ─
compute_elo_cols <- function(df_season, k = 20, init = 1500) {
  teams  <- unique(c(df_season$home_team, df_season$away_team))
  rating <- setNames(rep(init, length(teams)), teams)
  
  df_season <- df_season %>% arrange(game_date)
  
  home_pre  <- numeric(nrow(df_season))
  away_pre  <- numeric(nrow(df_season))
  
  for (i in seq_len(nrow(df_season))) {
    g  <- df_season[i, ]
    ht <- g$home_team
    at <- g$away_team
    rh <- rating[ht]      # 경기 전 홈 Elo
    ra <- rating[at]      # 경기 전 원정 Elo
    
    # 기록
    home_pre[i] <- rh
    away_pre[i] <- ra
    
    # 기대 승률
    exp_home <- 1 / (1 + 10 ^ ((ra - rh) / 400))
    res_home <- as.integer(g$home_runs > g$away_runs)
    
    # Elo 업데이트
    rating[ht] <- rh + k * (res_home - exp_home)
    rating[at] <- ra + k * ((1 - res_home) - (1 - exp_home))
  }
  
  df_season %>% 
    mutate(
      home_elo = home_pre,
      away_elo = away_pre,
      elo_diff = home_pre - away_pre   # 홈 – 원정
    )
}

# ─ 4. 시즌별 계산 후 결합 ─
games_with_elo <- bind_rows(
  compute_elo_cols(filter(games, season == 2024)),
  compute_elo_cols(filter(games, season == 2025))
)

raw_with_elo <- raw %>%
  left_join(
    games_with_elo %>%
      select(game_pk, home_elo, away_elo, elo_diff),
    by = "game_pk"
  )

final_raw <- raw_with_elo %>% 
  group_by(game_pk) %>% 
  arrange(game_pk) %>%                    # 같은 game_pk 안에서 원래 행 순서 유지
  mutate(
    is_home   = if_else(row_number() == 1, 1L, 0L),   # 첫 행 → 홈팀
    team_name = if_else(is_home == 1L, home_team, away_team)
  ) %>% 
  ungroup()


##############################
##### outlier_score 추가 #####
##############################

final_data_dedup <- final_data %>%
  distinct(game_pk, team_name, .keep_all = TRUE) %>%
  select(game_pk, team_name, outlier_score)

merged <- final_raw %>%
  left_join(final_data_dedup, by = c("game_pk", "team_name"))
merged <- merged[,-1]
merged <- merged %>% drop_na()

##############################
########## Modeling ##########
##############################
# ─ 데이터 로드 및 전처리 ─
merged <- merged %>%
  clean_names() %>%
  mutate(
    game_date = as.Date(game_date_x),
    season    = year(game_date_x),
    is_home   = as.integer(is_home),
    team_name = factor(team_name),
    runs      = if_else(is_home == 1, home_runs, away_runs),
    opp_runs  = if_else(is_home == 1, away_runs, home_runs),
    win       = as.integer(runs > opp_runs)
  )

# ─ 롤링 지표 계산 ─
merged <- merged %>%
  arrange(team_name, game_date) %>%
  group_by(team_name) %>%
  mutate(
    rolling_runs_5    = slide_dbl(lag(runs), mean, .before = 4, .complete = TRUE),
    rolling_xwoba_5   = slide_dbl(lag(xwoba), mean, .before = 4, .complete = TRUE),
    rolling_hardhit_5 = slide_dbl(lag(hardhit_percent), mean, .before = 4, .complete = TRUE)
  ) %>%
  ungroup() %>%
  mutate(across(starts_with("rolling_"), ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)))

# ─ 데이터 분할 ─
train_df <- merged %>% filter(season == 2024)
test_df  <- merged %>% filter(season == 2025, game_date <= as.Date("2025-05-22"))

# ─ 로지스틱 회귀 (승리 확률) ─
logit_fit <- glm(
  win ~ velocity + spin_rate + release_extension + xwoba + woba +
    launch_speed + hardhit_percent + barrels_per_pa_percent + elo_diff +
    park_factor + is_home + rolling_runs_5 + rolling_xwoba_5 + rolling_hardhit_5,
  family = binomial(link = "logit"),
  data   = train_df
)

# ─ train/test에 win_prob 추가 ─
train_df <- train_df %>%
  mutate(win_prob = predict(logit_fit, newdata = ., type = "response"))

test_df <- test_df %>%
  mutate(win_prob = predict(logit_fit, newdata = ., type = "response"))

# ─ ZINB 모델 적합 ─
zinb_fit <- zeroinfl(
  runs ~ velocity + spin_rate + release_extension + api_break_z_with_gravity +
    pitcher_run_exp + xwoba + woba + launch_speed + hardhit_percent +
    barrels_per_pa_percent + park_factor + elo_diff + outlier_score + is_home +
    win_prob + rolling_runs_5 + rolling_xwoba_5 + rolling_hardhit_5 |
    is_home + win_prob,
  dist = "negbin",
  EM   = TRUE,
  data = train_df
)

# - 적합 결과 -
test_pred <- predict(zinb_fit, newdata=test_df, type="response")
cat("RMSE =", RMSE(test_pred, test_df$runs), "\n")
cat("MAE  =", MAE (test_pred, test_df$runs), "\n")


# ─ 예측 대상 경기 추출 ─
target_game_pks <- merged %>%
  filter(
    season == 2025,
    game_date >= as.Date("2025-05-23"),
    game_date <= as.Date("2025-06-01"),
    team_name %in% c("San Francisco Giants", "New York Yankees")
  ) %>%
  pull(game_pk) %>%
  unique()

# ─ 해당 game_pk의 전체 팀 데이터 추출 및 예측 ─
pred_df <- merged %>%
  filter(game_pk %in% target_game_pks) %>%
  mutate(win_prob = predict(logit_fit, newdata = ., type = "response"))

# win_prob가 만들어진 이후 zinb 예측
pred_df <- pred_df %>%
  mutate(
    pred_runs = predict(zinb_fit, newdata = ., type = "response"),
    opponent  = if_else(is_home == 1, away_team, home_team)
  )


# ─ 홈팀/원정팀 나눠서 한 줄로 정리 ─
home_df <- pred_df %>%
  filter(is_home == 1) %>%
  select(game_pk, game_date,
         home_team = team_name,
         away_team = opponent,
         home_pred = pred_runs)

away_df <- pred_df %>%
  filter(is_home == 0) %>%
  select(game_pk,
         away_pred = pred_runs)

result_df <- home_df %>%
  inner_join(away_df, by = "game_pk") %>%
  arrange(game_date) %>%
  mutate(
    winner_pred = case_when(
      home_pred > away_pred ~ home_team,
      home_pred < away_pred ~ away_team,
      TRUE ~ "Tie"
    )
  )
