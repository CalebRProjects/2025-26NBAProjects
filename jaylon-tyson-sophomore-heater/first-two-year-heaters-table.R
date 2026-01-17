# tyson_style_first_two_seasons.R
# ------------------------------------------------------------
# Purpose:
#   Find rare "Tyson-style" games and render a logo table.
#
# Definition (default):
#   - Player is in career year 1 or 2 (rookie/sophomore seasons)
#   - PTS >= pts_min
#   - TS  >= ts_cut
#   - 3PM >= fg3m_min
#   - Season GP >= gp_min (games played in that season, from league gamelog rows)
#
# Notes:
#   - NBA Stats leaguegamelog coverage is strongest from ~1996-97 onward.
#   - Uses caching per season to avoid repeated pulls.
#   - Outputs:
#       data/tyson_style_games.csv
#       figs/tyson_style_table.png
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(hoopR)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(gt)
  library(gtExtras)
  library(readr)
  library(fs)
})

# -----------------------------
# Helpers
# -----------------------------

find_first_df <- function(x) {
  if (inherits(x, "data.frame")) return(x)
  if (is.list(x)) {
    for (i in seq_along(x)) {
      out <- find_first_df(x[[i]])
      if (!is.null(out)) return(out)
    }
  }
  NULL
}

calc_ts <- function(pts, fga, fta) {
  pts / (2 * (fga + 0.44 * fta))
}

season_strings <- function(start_season, end_season) {
  # start_season/end_season are start years (e.g., 1996 -> "1996-97")
  paste0(start_season:end_season, "-", substr(start_season:end_season + 1, 3, 4))
}

pull_league_gamelog <- function(season, season_type, cache_dir, sleep_sec = 0.75) {
  cache_path <- path(cache_dir, paste0("lg_", season, ".rds"))
  
  if (file_exists(cache_path)) {
    message("Cache hit: ", season)
    return(readRDS(cache_path))
  }
  
  message("Pulling ", season, " ...")
  Sys.sleep(sleep_sec)
  
  resp <- tryCatch(
    nba_leaguegamelog(player_or_team = "P", season = season, season_type = season_type),
    error = function(e) {
      message("Failed ", season, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(tibble())
  
  tbl <- find_first_df(resp)
  if (is.null(tbl) || nrow(tbl) == 0) return(tibble())
  
  out <- tbl |>
    transmute(
      season_id = SEASON_ID,                  # e.g., "22025"
      player_id = as.character(PLAYER_ID),
      player    = PLAYER_NAME,
      team      = TEAM_ABBREVIATION,
      game_id   = GAME_ID,
      game_date = as.Date(GAME_DATE),
      min  = as.numeric(MIN),
      pts  = as.numeric(PTS),
      fga  = as.numeric(FGA),
      fta  = as.numeric(FTA),
      fg3m = as.numeric(FG3M),
      fg3a = as.numeric(FG3A),
      ts   = calc_ts(pts, fga, fta)
    )
  
  saveRDS(out, cache_path)
  out
}

get_team_logos <- function() {
  # Prefer nba_teams(); fallback to espn_nba_teams()
  teams_nba <- tryCatch(hoopR::nba_teams(), error = function(e) NULL)
  logo_nba <- if (!is.null(teams_nba)) {
    teams_nba |>
      distinct(team_abbreviation, logo) |>
      rename(team = team_abbreviation, logo_url = logo)
  } else {
    tibble(team = character(), logo_url = character())
  }
  
  teams_espn <- tryCatch(hoopR::espn_nba_teams(), error = function(e) NULL)
  logo_espn <- if (!is.null(teams_espn)) {
    teams_espn |>
      distinct(abbreviation, logo) |>
      rename(team = abbreviation, logo_url = logo)
  } else {
    tibble(team = character(), logo_url = character())
  }
  
  logo_nba |>
    full_join(logo_espn, by = "team", suffix = c("_nba", "_espn")) |>
    transmute(team, logo_url = coalesce(logo_url_nba, logo_url_espn))
}

# -----------------------------
# Settings
# -----------------------------

start_season    <- 1996
end_season      <- 2025
include_current <- TRUE
current_season  <- "2025-26"
season_type     <- "Regular Season"

# Filter thresholds
pts_min  <- 35
ts_cut   <- 0.90
fg3m_min <- 7
gp_min   <- 20
career_year_max <- 2   # first two seasons

# Rate limit + cache
sleep_sec <- 0.75
cache_dir <- "nba_lg_cache"

# Outputs
out_dir_data <- "data"
out_dir_figs <- "figs"
out_csv      <- path(out_dir_data, "tyson_style_games.csv")
out_png      <- path(out_dir_figs, "tyson_style_table.png")

dir_create(cache_dir)
dir_create(out_dir_data)
dir_create(out_dir_figs)

# -----------------------------
# Build league dataset (cached)
# -----------------------------

seasons <- season_strings(start_season, end_season)
if (include_current) seasons <- c(seasons, current_season)

all_games <- map_dfr(
  seasons,
  ~ pull_league_gamelog(.x, season_type = season_type, cache_dir = cache_dir, sleep_sec = sleep_sec)
)

# Basic hygiene
all_games <- all_games |>
  filter(!is.na(game_id), !is.na(player_id), !is.na(season_id)) |>
  filter(is.finite(ts), is.finite(pts), is.finite(fg3m)) |>
  mutate(season_num = as.integer(season_id))

# Games played per player-season (from logs)
player_gp <- all_games |>
  group_by(season_id, player_id) |>
  summarise(season_gp = n(), .groups = "drop")

# Career year per player-season (rookie=1, sophomore=2, etc.)
player_seasons <- all_games |>
  distinct(player_id, season_num) |>
  arrange(player_id, season_num) |>
  group_by(player_id) |>
  mutate(career_year = row_number()) |>
  ungroup()

# -----------------------------
# Filter: first two seasons
# -----------------------------

tyson_style <- all_games |>
  left_join(player_seasons, by = c("player_id", "season_num")) |>
  left_join(player_gp, by = c("season_id", "player_id")) |>
  filter(
    career_year <= career_year_max,
    season_gp >= gp_min,
    pts >= pts_min,
    ts >= ts_cut,
    fg3m >= fg3m_min
  ) |>
  distinct(player_id, game_id, .keep_all = TRUE) |>
  arrange(desc(ts), desc(pts), desc(fg3m))

# Save a tidy data extract for GitHub
tyson_style |>
  transmute(
    team,
    player,
    game_date,
    pts,
    fg3m,
    fg3a,
    ts_pct = 100 * ts,
    season_gp,
    career_year,
    season_id,
    player_id,
    game_id
  ) |>
  write_csv(out_csv)

# -----------------------------
# Build GT table with logos
# -----------------------------

logos <- get_team_logos()

tyson_key <- "jaylon tyson"

tbl <- tyson_style |>
  transmute(
    Team   = team,
    Player = player,
    Date   = game_date,
    PTS    = pts,
    `3PM`  = fg3m,
    `3PA`  = fg3a,
    `TS%`  = 100 * ts,
    `Season GP` = season_gp,
    Logo = team
  ) |>
  left_join(logos, by = c("Team" = "team")) |>
  mutate(Logo = logo_url) |>
  select(Team, Logo, Player, Date, PTS, `3PM`, `3PA`, `TS%`, `Season GP`)

subtitle_txt <- paste0(
  "Filter: PTS ≥ ", pts_min,
  ", TS ≥ ", ts_cut * 100, "%, 3PM ≥ ", fg3m_min,
  ", season GP ≥ ", gp_min,
  ", career years 1–", career_year_max
)

gt_tbl <- tbl |>
  gt() |>
  gtExtras::gt_img_rows(Logo, height = 22) |>
  cols_label(Logo = "") |>
  cols_align(
    align = "center",
    columns = c(Team, Logo, Date, PTS, `3PM`, `3PA`, `TS%`, `Season GP`)
  ) |>
  fmt_number(columns = c(`TS%`), decimals = 1) |>
  tab_style(
    style = list(cell_fill(color = "#FFF3B0"), cell_text(weight = "bold")),
    locations = cells_body(rows = str_detect(str_to_lower(Player), tyson_key))
  ) |>
  tab_header(
    title = md("**Tyson-style games (first two seasons)**"),
    subtitle = md(subtitle_txt)
  ) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(6)
  )

gt_tbl

# Export table image for posting/GitHub
gtsave(gt_tbl, out_png)

message("Wrote: ", out_csv)
message("Wrote: ", out_png)