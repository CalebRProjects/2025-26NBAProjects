# ============================================================
# F5-STYLE AGING CONTEXT TABLE FOR LEBRON
# One-table version with stronger rTS coloring and team logos
# ============================================================

library(dplyr)
library(gt)
library(tibble)
library(scales)

# ============================================================
# LEAGUE-AVERAGE TS% BY SEASON
# ============================================================

season_ts <- tribble(
  ~Season, ~lg_ts,
  "2025-26", 0.581,
  "2024-25", 0.576,
  "2023-24", 0.580,
  "2022-23", 0.581,
  "2021-22", 0.566,
  "2020-21", 0.572,
  "2019-20", 0.565,
  "2018-19", 0.560,
  "2017-18", 0.556,
  "2016-17", 0.552,
  "2015-16", 0.541,
  "2014-15", 0.534,
  "2013-14", 0.541,
  "2012-13", 0.535,
  "2011-12", 0.527,
  "2003-04", 0.516,
  "2002-03", 0.519,
  "1991-92", 0.531,
  "1973-74", 0.503,
  "1972-73", 0.498
)

# ============================================================
# TEAM LOGO URLS
# ============================================================

team_logo_urls <- c(
  LAL = "https://a.espncdn.com/i/teamlogos/nba/500/lal.png",
  GSW = "https://a.espncdn.com/i/teamlogos/nba/500/gs.png",
  LAC = "https://a.espncdn.com/i/teamlogos/nba/500/lac.png",
  BOS = "https://a.espncdn.com/i/teamlogos/nba/500/bos.png",
  CLE = "https://a.espncdn.com/i/teamlogos/nba/500/cle.png"
)

# ============================================================
# SOURCE TABLE (Statmuse.com)
# Oldest age-35+ seasons with 20+ PPG and 6+ APG
# ============================================================

oldest_20_6 <- tribble(
  ~Player, ~Age, ~PPG, ~APG, ~Season, ~TM, ~GP, ~MPG, ~RPG, ~SPG, ~BPG, ~`FG%`, ~`3P%`, ~`FT%`, ~`TS%`,
  "LeBron James", 41, 20.9, 7.0, "2025-26", "LAL", 54, 33.6, 6.0, 1.1, 0.6, 51.2, 31.4, 74.2, 59.0,
  "LeBron James", 40, 24.4, 8.2, "2024-25", "LAL", 70, 34.9, 7.8, 1.0, 0.6, 51.3, 37.6, 78.2, 60.4,
  "LeBron James", 39, 25.7, 8.3, "2023-24", "LAL", 71, 35.3, 7.3, 1.3, 0.5, 54.0, 41.0, 75.0, 63.0,
  "LeBron James", 38, 28.9, 6.8, "2022-23", "LAL", 55, 35.5, 8.3, 0.9, 0.6, 50.0, 32.1, 76.8, 58.3,
  "LeBron James", 37, 30.3, 6.2, "2021-22", "LAL", 56, 37.2, 8.2, 1.3, 1.1, 52.4, 35.9, 75.6, 61.9,
  "James Harden", 36, 23.8, 8.2, "2025-26", "CLE", 65, 35.1, 5.0, 1.1, 0.4, 43.4, 37.2, 88.4, 61.1,
  "Stephen Curry", 36, 24.5, 6.0, "2024-25", "GSW", 70, 32.2, 4.4, 1.1, 0.4, 44.8, 39.7, 93.3, 61.8,
  "LeBron James", 36, 25.0, 7.8, "2020-21", "LAL", 45, 33.4, 7.7, 1.1, 0.6, 51.3, 36.5, 69.8, 60.2,
  "James Harden", 35, 22.8, 8.7, "2024-25", "LAC", 79, 35.3, 5.8, 1.5, 0.7, 41.0, 35.2, 87.4, 58.2,
  "LeBron James", 35, 25.3, 10.2, "2019-20", "LAL", 67, 34.6, 7.8, 1.2, 0.5, 49.3, 34.8, 69.3, 57.7,
  "Larry Bird", 35, 20.2, 6.8, "1991-92", "BOS", 45, 36.9, 9.6, 0.9, 0.7, 46.6, 40.6, 92.6, 54.7,
  "Jerry West", 35, 20.3, 6.6, "1973-74", "LAL", 31, 31.2, 3.7, 2.6, 0.7, 44.7, NA, 83.3, 51.9,
  "Lenny Wilkens", 35, 20.5, 8.4, "1972-73", "CLE", 75, 39.6, 4.6, NA, NA, 44.9, NA, 82.8, 51.8
) %>%
  mutate(
    TM = case_when(
      Player == "James Harden" & Season == "2025-26" ~ "CLE",
      TRUE ~ TM
    )
  ) %>%
  left_join(season_ts, by = "Season") %>%
  mutate(
    rTS = (`TS%` / 100 - lg_ts) * 100,
    TM_logo = unname(team_logo_urls[TM])
  ) %>%
  arrange(desc(Age), desc(PPG)) %>%
  mutate(Rk = row_number())

# ============================================================
# DISPLAY TABLE
# ============================================================

lebron_count_20_6 <- sum(oldest_20_6$Player == "LeBron James")
total_count_20_6 <- nrow(oldest_20_6)

oldest_20_6_display <- oldest_20_6 %>%
  select(Rk, Player, Age, Season, TM_logo, GP, PPG, RPG, APG, `TS%`, rTS)

tab_20_6 <- oldest_20_6_display %>%
  gt() %>%
  tab_header(
    title = md("**Oldest age-35+ seasons with 20+ PPG and 6+ APG**"),
    subtitle = md(
      paste0(
        "LeBron James owns **",
        lebron_count_20_6,
        "** of these **",
        total_count_20_6,
        "** seasons"
      )
    )
  ) %>%
  fmt_number(
    columns = c(PPG, RPG, APG, `TS%`, rTS),
    decimals = 1
  ) %>%
  sub_missing(columns = everything(), missing_text = "—") %>%
  cols_label(
    Rk = "Rk",
    TM_logo = "",
    GP = "GP",
    PPG = "PPG",
    RPG = "RPG",
    APG = "APG",
    `TS%` = "TS%",
    rTS = "rTS%"
  ) %>%
  tab_spanner(
    label = "Production",
    columns = c(PPG, RPG, APG)
  ) %>%
  tab_spanner(
    label = "Efficiency",
    columns = c(`TS%`, rTS)
  ) %>%
  text_transform(
    locations = cells_body(columns = TM_logo),
    fn = function(x) web_image(url = x, height = 20)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f4cccc"),
    locations = cells_body(columns = rTS, rows = rTS <= -2)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fce5cd"),
    locations = cells_body(columns = rTS, rows = rTS > -2 & rTS < 0)
  ) %>%
  tab_style(
    style = cell_fill(color = "#d9ead3"),
    locations = cells_body(columns = rTS, rows = rTS >= 0 & rTS < 1)
  ) %>%
  tab_style(
    style = cell_fill(color = "#b6d7a8"),
    locations = cells_body(columns = rTS, rows = rTS >= 1 & rTS < 3)
  ) %>%
  tab_style(
    style = cell_fill(color = "#6aa84f"),
    locations = cells_body(columns = rTS, rows = rTS >= 3)
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = Player == "LeBron James")
  ) %>%
  cols_align(
    align = "center",
    columns = c(Rk, Age, Season, TM_logo, GP, PPG, RPG, APG, `TS%`, rTS)
  ) %>%
  cols_width(
    Player ~ px(170),
    TM_logo ~ px(36)
  ) %>%
  opt_row_striping() %>%
  tab_source_note(
    source_note = md(
      "rTS% is relative to the league-average true shooting percentage for that season. LeBron stands alone."
    )
  )

# Print table
tab_20_6
