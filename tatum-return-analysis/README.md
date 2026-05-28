# Jayson Tatum Has Been an Anomaly

## Overview

This project evaluates Jayson Tatum’s return from an Achilles rupture during the 2025–26 season. The analysis focuses on how much of his pre-injury role and impact returned immediately, where the scoring efficiency lagged, and how his rebounding, playmaking, and lineup impact helped stabilize Boston. It frames Tatum’s season as less of a finished comeback and more of a live ramp-up under playoff pressure.

## Main Question

How impactful has Jayson Tatum been less than a year after his Achilles rupture, and what parts of his game have already returned versus what still needs to catch up?

## Data Sources

- NBA.com
- hoopR / NBA Stats API
- Databallr
- ESPN Analytics
- Basketball Reference
- Manually entered playoff, rebounding, Game 7, and Net Points data

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- janitor
- lubridate
- kableExtra
- gt
- showtext / sysfonts
- zoo

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Regular-season pre/post-return comparison table
- Post-return game log trend chart
- Tatum on/off impact table
- Post-return rebounding leaderboard
- Defensive rebounding percentage chart
- Playmaking indicators table
- Usage vs. true shooting league-context chart
- Playoff profile table
- Playoff ranking table
- Celtics playoff Net Points chart
- Game 7 performance tables

## Notes and Limitations

This project combines live NBA Stats API pulls with manually entered source extracts. The game-log and league-context visuals depend on `hoopR`, so those sections may fail or render fallback plots if the NBA Stats API is unavailable, rate-limited, or blocked. Several tables use manually entered data from Databallr, ESPN Analytics, Basketball Reference, and NBA.com, so the report is not fully reproducible from raw public data alone. The analysis is also built around a small post-return and playoff sample, so shooting efficiency and lineup impact should be interpreted with injury timeline, role context, and opponent context in mind.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
