# Cam Johnson: Second-Half Rebound

## Overview

This project evaluates Cam Johnson’s second-half improvement with Denver during the 2025–26 season. The analysis focuses on whether his production jump reflects a real adjustment in role comfort and shot quality, or whether it is mostly short-run shooting variance. It uses game-log splits, rolling trends, and shot-chart data to compare Johnson’s early-season profile with his recent stretch.

## Main Question

Has Cam Johnson’s recent production spike come from a meaningful role/shot-profile improvement, or mostly better results from the same offensive blueprint?

## Data Sources

- NBA.com
- hoopR / NBA Stats API
- NBA ShotChartDetail

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- lubridate
- kableExtra
- ggrepel
- showtext / sysfonts

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Rolling REB/36, TS%, and 3PA share chart
- Early vs. recent role/usage proxy table
- First-half vs. second-half per-game table
- Three-point zone accuracy chart
- Shot diet by distance bucket chart

## Notes and Limitations

This report depends on live NBA Stats API pulls through `hoopR`, including league game logs and ShotChartDetail data. If NBA.com changes endpoint schemas, blocks requests, or rate-limits calls, the report may require small code adjustments or cached/local data. The Early/Recent split is based on games played rather than lineup context, opponent quality, or injury context. Usage rate is not available at a game-by-game level in this workflow, so the report uses usage-event proxies instead.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
