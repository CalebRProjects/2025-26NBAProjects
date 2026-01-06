# Anfernee Simons Last-N Games Snapshot (2025–26)
## Overview
This script generates a quick “last N games” performance snapshot for Anfernee Simons during the 2025–26 regular season. It pulls game logs from the NBA Stats API via hoopR, merges base and advanced measures, computes per-game averages across the most recent games, and exports a styled table as a PNG.

# What it outputs
A one-row table of last-N averages for:
* MIN, PTS, REB, AST, TOV, 3PM
* TS%
* NET RTG

A saved image: simons_last<N>_avgs.png

# Data sources
* NBA Stats API (via hoopR)
  * NBA game player logs for base & advanced stats

# Author
Caleb Ramsey - Statistics & Sports Media & Analytics (Virginia Tech)
