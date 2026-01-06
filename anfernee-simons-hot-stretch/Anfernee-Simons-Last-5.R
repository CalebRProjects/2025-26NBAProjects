library(dplyr)
library(tidyr)
library(gt)
library(hoopR)
library(webshot2)

player_id   <- 1629014
season_used <- "2025-26"
n_games     <- 5

celtics_green <- "#007A33"
celtics_gold  <- "#BA9653"

to_num <- function(x)
{
  suppressWarnings(as.numeric(gsub("[^0-9\\.-]", "", as.character(x))))
}

first_df <- function(x)
{
  x[vapply(x, is.data.frame, logical(1))][[1]]
}

# Game logs
base_df <- hoopR::nba_playergamelogs(
  player_id    = player_id,
  season       = season_used,
  season_type  = "Regular Season",
  measure_type = "Base"
) %>%
  first_df() %>%
  mutate(GAME_DATE = as.Date(GAME_DATE)) %>%
  arrange(desc(GAME_DATE))

adv_df <- hoopR::nba_playergamelogs(
  player_id    = player_id,
  season       = season_used,
  season_type  = "Regular Season",
  measure_type = "Advanced"
) %>%
  first_df() %>%
  mutate(GAME_DATE = as.Date(GAME_DATE)) %>%
  arrange(desc(GAME_DATE))

stopifnot(nrow(base_df) > 0, nrow(adv_df) > 0)

# Last N games
lastN_base <- base_df %>%
  slice_head(n = n_games) %>%
  transmute(
    GAME_ID,
    GAME_DATE,
    MATCHUP,
    MIN = to_num(MIN),
    PTS = to_num(PTS),
    REB = to_num(REB),
    AST = to_num(AST),
    TOV = to_num(TOV),
    `3PM` = to_num(FG3M)
  )

lastN_adv <- adv_df %>%
  transmute(
    GAME_ID,
    NET_RTG = to_num(NET_RATING),
    TS_PCT  = to_num(TS_PCT)
  )

lastN <- lastN_base %>%
  left_join(lastN_adv, by = "GAME_ID")

# Averages
avgs <- lastN %>%
  summarise(
    MIN = mean(MIN, na.rm = TRUE),
    PTS = mean(PTS, na.rm = TRUE),
    REB = mean(REB, na.rm = TRUE),
    AST = mean(AST, na.rm = TRUE),
    TOV = mean(TOV, na.rm = TRUE),
    `3PM` = mean(`3PM`, na.rm = TRUE),
    `TS%` = mean(if_else(TS_PCT <= 1, TS_PCT * 100, TS_PCT), na.rm = TRUE),
    `NET RTG` = mean(NET_RTG, na.rm = TRUE)
  )

# GT table
gt_tbl <- gt(avgs) %>%
  tab_header(
    title = md("**Anfernee Simons**"),
    subtitle = md(paste0("Last ", n_games, " games averages (", season_used, ")"))
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  cols_width(
    MIN ~ px(90),
    PTS ~ px(90),
    REB ~ px(90),
    AST ~ px(90),
    TOV ~ px(90),
    `3PM` ~ px(90),
    `TS%` ~ px(95),
    `NET RTG` ~ px(110)
  ) %>%
  tab_options(
    table.width = px(900),
    data_row.padding = px(8),
    heading.background.color = celtics_green,
    column_labels.background.color = celtics_green,
    table.border.top.color = celtics_green,
    table.border.bottom.color = celtics_green,
    table.font.names = "Arial",
    table.font.size = px(18)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = celtics_green),
      cell_text(color = "white", weight = "bold")
    ),
    locations = list(
      cells_title(groups = c("title", "subtitle")),
      cells_column_labels(everything())
    )
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = celtics_gold, weight = px(2)),
    locations = cells_column_labels(everything())
  )

gt_tbl

gtsave(
  gt_tbl,
  filename = paste0("simons_last", n_games, "_avgs.png"),
  expand = 10
)