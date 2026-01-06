library(hoopR)
library(dplyr)
library(gt)
library(readr)
library(webshot2)

player_id <- 1628991
seasons <- c("2018-19","2019-20","2020-21","2021-22","2022-23","2023-24","2024-25","2025-26")

# Helpers
get_season_logs <- function(season) {
  out <- nba_playergamelogs(
    player_id   = player_id,
    season      = season,
    season_type = "Regular Season"
  )
  df <- out[sapply(out, is.data.frame)][[1]]
  df$SEASON_YEAR <- season
  df$SEASON_TYPE <- "Regular Season"
  df
}

get_season_logs_playoffs <- function(season) {
  out <- nba_playergamelogs(
    player_id   = player_id,
    season      = season,
    season_type = "Playoffs"
  )
  df <- out[sapply(out, is.data.frame)][[1]]
  df$SEASON_YEAR <- season
  df$SEASON_TYPE <- "Playoffs"
  df
}

# Pull all games
jjj_games_all <- bind_rows(
  lapply(seasons, function(s) {
    Sys.sleep(0.7)
    get_season_logs(s)
  }),
  lapply(seasons, function(s) {
    Sys.sleep(0.7)
    get_season_logs_playoffs(s)
  })
)

# Filter games where STOCKS > REB
jjj_stocks_gt_reb_all <- jjj_games_all %>%
  mutate(
    game_date  = as.Date(GAME_DATE),
    MIN        = round(parse_number(MIN), 1),
    REB        = as.numeric(REB),
    STL        = as.numeric(STL),
    BLK        = as.numeric(BLK),
    PTS        = as.numeric(PTS),
    AST        = as.numeric(AST),
    TOV        = as.numeric(TOV),
    PLUS_MINUS = as.numeric(PLUS_MINUS),
    stocks     = STL + BLK
  ) %>%
  filter(!is.na(stocks), !is.na(REB), stocks > REB) %>%
  arrange(game_date) %>%
  select(
    game_date,
    SEASON_YEAR,
    SEASON_TYPE,
    MATCHUP,
    MIN,
    REB,
    STL,
    BLK,
    stocks,
    PTS,
    AST,
    TOV,
    PLUS_MINUS
  )

total_games <- nrow(jjj_stocks_gt_reb_all)

# Season Summary
season_summary <- jjj_stocks_gt_reb_all %>%
  group_by(SEASON_YEAR) %>%
  summarise(
    games_total = n(),
    reg_season  = sum(SEASON_TYPE == "Regular Season"),
    playoffs    = sum(SEASON_TYPE == "Playoffs"),
    .groups = "drop"
  ) %>%
  arrange(SEASON_YEAR)

season_summary_with_total <- bind_rows(
  season_summary,
  tibble(
    SEASON_YEAR = "Career",
    games_total = sum(season_summary$games_total, na.rm = TRUE),
    reg_season  = sum(season_summary$reg_season,  na.rm = TRUE),
    playoffs    = sum(season_summary$playoffs,    na.rm = TRUE)
  )
)

# Tables
season_summary_gt <- season_summary_with_total %>%
  gt() %>%
  tab_header(
    title = "Jaren Jackson Jr. Career Games",
    subtitle = "Games with STOCKS (STL+BLK) > Rebounds"
  ) %>%
  cols_label(
    SEASON_YEAR = "Season",
    games_total = "Total",
    reg_season  = "Regular Season",
    playoffs    = "Playoffs"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = SEASON_YEAR == "Career")
  )

game_list_gt <- jjj_stocks_gt_reb_all %>%
  arrange(desc(stocks), game_date) %>%
  gt() %>%
  tab_header(
    title = "Game Log",
    subtitle = "JJJ games where STOCKS (STL+BLK) > Rebounds"
  ) %>%
  fmt_date(columns = game_date, date_style = 3) %>%
  fmt_number(columns = c(MIN, REB, STL, BLK, stocks, PTS, AST, TOV, PLUS_MINUS), decimals = 0) %>%
  cols_label(
    game_date   = "Date",
    SEASON_YEAR = "Season",
    SEASON_TYPE = "Type",
    MATCHUP     = "Matchup",
    MIN         = "MIN",
    REB         = "REB",
    STL         = "STL",
    BLK         = "BLK",
    stocks      = "Stocks",
    PTS         = "PTS",
    AST         = "AST",
    TOV         = "TOV",
    PLUS_MINUS  = "+/-"
  )

# Exports
gtsave(season_summary_gt, "jjj_stocks_gt_reb_season_summary.png")
gtsave(game_list_gt, "jjj_stocks_gt_reb_game_list.png")

write.csv(
  jjj_stocks_gt_reb_all,
  "jjj_stocks_gt_reb_game_list.csv",
  row.names = FALSE
)

cat("Total qualifying games:", total_games, "\n")
