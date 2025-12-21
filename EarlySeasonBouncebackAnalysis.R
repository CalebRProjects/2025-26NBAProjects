# ============================================================
# 0) Libraries
# ============================================================
library(dplyr)
library(lubridate)
library(hoopR)
library(tibble)

# ============================================================
# 1) Pull league-wide player game logs (2025-26)
# ============================================================
season <- "2025-26"

lg <- hoopR::nba_leaguegamelog(
  season = season,
  season_type = "Regular Season",
  player_or_team = "P"
)

lg_df <- lg$LeagueGameLog
glimpse(lg_df)

# ============================================================
# 2) Build games_df (player-game rows) + TS%
# ============================================================
games_df <- lg_df %>%
  transmute(
    player_id = as.character(PLAYER_ID),
    player_name = PLAYER_NAME,              # keep name here to avoid later joins
    game_id   = as.character(GAME_ID),
    game_date = as.Date(GAME_DATE),
    
    minutes   = as.numeric(MIN),
    pts = as.numeric(PTS),
    fga = as.numeric(FGA),
    fta = as.numeric(FTA),
    
    ts = if_else(2 * (fga + 0.44 * fta) > 0,
                 pts / (2 * (fga + 0.44 * fta)),
                 NA_real_)
  ) %>%
  arrange(player_id, game_date)

games_df %>% summarise(players = n_distinct(player_id), rows = n())

# ============================================================
# 3) Baseline table (ts_base). Do NOT use win here.
#    Replace this later with true career TS if you want.
# ============================================================
baseline_df <- games_df %>%
  group_by(player_id) %>%
  summarise(
    ts_base = mean(ts, na.rm = TRUE),
    .groups = "drop"
  )

baseline_df %>% summarise(players_with_baseline = n())

# ============================================================
# 4) Windowing (early vs recent) + player-level aggregates
#    This is where you compute early/recent TS and PPG.
# ============================================================
# ============================================================
# 4) Windowing (player-specific first half vs second half so far)
# ============================================================
min_games <- 20
min_half  <- 10   # ensures each half has enough games

player_windows <- games_df %>%
  inner_join(baseline_df, by = "player_id") %>%
  group_by(player_id, player_name) %>%
  arrange(game_date, .by_group = TRUE) %>%
  mutate(
    gp_full = n(),
    cut = floor(gp_full / 2),
    idx = row_number(),
    win = case_when(
      idx <= cut ~ "early",
      idx >  cut ~ "recent",
      TRUE ~ "mid"
    )
  ) %>%
  filter(gp_full >= min_games, cut >= min_half, (gp_full - cut) >= min_half) %>%
  summarise(
    gp = first(gp_full),
    early_n = first(cut),
    recent_n = first(gp_full - cut),
    mpg = mean(minutes, na.rm = TRUE),
    
    early_ts  = weighted.mean(ts[win == "early"],  w = minutes[win == "early"],  na.rm = TRUE),
    recent_ts = weighted.mean(ts[win == "recent"], w = minutes[win == "recent"], na.rm = TRUE),
    
    early_ppg  = weighted.mean(pts[win == "early"],  w = minutes[win == "early"],  na.rm = TRUE),
    recent_ppg = weighted.mean(pts[win == "recent"], w = minutes[win == "recent"], na.rm = TRUE),
    
    ts_base = first(ts_base),
    
    ts_vs_base = early_ts - ts_base,
    ts_delta   = recent_ts - early_ts,
    ppg_delta  = recent_ppg - early_ppg,
    
    .groups = "drop"
  )

# ============================================================
# 5) Scoring + flagging
#    TS-based BSS + RS. PPG stays as a presentation stat for now.
# ============================================================
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

results <- player_windows %>%
  mutate(
    BSS = z(ts_vs_base),          # bad start: low BSS
    RS  = z(ts_delta),            # rebound: high RS
    PSI = 1,                      # placeholder until you add usage/shot-mix stability
    FLAG = (BSS <= -1) & (RS >= 1) & (PSI >= 0.7)
  ) %>%
  arrange(desc(FLAG), desc(RS))

results %>% summarise(flagged = sum(FLAG, na.rm = TRUE), total = n())

# ============================================================
# 6) Output tables (ready for Twitter selection)
# ============================================================
flags <- results %>% filter(FLAG)

flags %>%
  select(player_name, player_id, gp, mpg,
         early_ts, recent_ts, ts_base, ts_delta,
         early_ppg, recent_ppg, ppg_delta,
         BSS, RS) %>%
  arrange(desc(RS)) %>%
  head(25)

library(dplyr)

table_out_pretty <- table_out %>%
  mutate(
    mpg = round(mpg, 1),
    early_ts = round(early_ts, 3),
    recent_ts = round(recent_ts, 3),
    early_ppg = round(early_ppg, 1),
    recent_ppg = round(recent_ppg, 1),
    ts_delta = round(ts_delta, 3),
    ppg_delta = round(ppg_delta, 1),
    BSS = round(BSS, 2),
    RS = round(RS, 2)
  )

table_out_pretty

