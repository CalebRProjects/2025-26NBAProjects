# Anthony Davis in Washington: A Buy-Low Bet on Top-75 Talent

## Overview

This project evaluates Anthony Davis as a buy-low acquisition for Washington, focusing on his recent offensive role, shot profile, defensive event creation, rim protection, and roster fit. The analysis frames Davis as a high-upside veteran addition whose value depends less on self-creation and more on rim pressure, defensive coverage, screening, and frontcourt stability next to Alex Sarr. It also weighs the upside of adding top-end talent against the obvious availability and age-related risks.

## Main Question

Can Anthony Davis still provide enough two-way value to accelerate Washington’s competitive timeline if his role is simplified around play finishing, defense, and frontcourt stability?

## Data Sources

- NBA.com
- Synergy / SportsRadar
- Basketball Reference
- hoopR / NBA Stats API
- Manually entered Washington roster context

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- readxl
- janitor
- kableExtra
- hoopR
- showtext / sysfonts
- glue

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Anthony Davis season profile table
- Playtype possession-weighted summary table
- Playtype usage mix chart
- Playtype PPP trend chart
- Shot-zone FG% vs. league chart
- Stocks per game rank table
- Last-five-seasons stocks leaderboard
- Rim protection table
- Washington young core fit table

## Notes and Limitations

This analysis depends on two local Excel files for playtype and shot-zone data, so the report will only render if those files are available at the paths listed in the YAML parameters. Several inputs are manually entered, including season-level Davis stats, Washington roster context, league reference values, and the long-term stocks leaderboard. The defensive API sections depend on `hoopR`, so those tables may fail or return fallback notes if the NBA Stats API is unavailable, rate-limited, or returns a changed schema. The Washington fit section is contextual and projection-based, so it should be read as a roster-fit argument rather than a definitive forecast.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `AD-wizards-fit-analysis.pdf` — rendered report
- `README.md` — project overview
- `AD Scoring Zones (Last 5 Seasons).xlsx` — local shot-zone input
- `data/` — local data files
