# What Are We Watching With Victor Wembanyama?

## Overview

This project contextualizes Victor Wembanyama’s first playoff run through historical comparisons, signature performances, defensive impact, offensive development, and team context. The analysis focuses on why the run feels different from a typical young-star breakout, especially given the combination of scoring, rebounding, rim protection, and playoff responsibility. It frames Wembanyama’s postseason as an early but meaningful signal of an all-time trajectory, while still acknowledging the uncertainty of projecting greatness.

## Main Question

How historically unusual is Victor Wembanyama’s first playoff run, and does it justify discussing him as a potential all-time trajectory player this early?

## Data Sources

- Basketball Reference
- NBA.com
- PBPStats.com
- StatMuse

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- kableExtra
- showtext / sysfonts
- forcats

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Wembanyama first playoff run snapshot table
- First playoff run production comparison table
- First playoff run context table
- Defensive on/off impact table
- Wembanyama shot-profile table
- Signature playoff performances table
- Conference Finals debut scoring table
- 40-point, 20-rebound playoff leaderboard
- Playoff blocks leaderboard
- Youngest 40-point playoff games table
- Spurs support-context table

## Notes and Limitations

This report is built from manually entered game logs, leaderboard extracts, and source screenshots/tables, so the analysis is not fully reproducible from raw public data alone. The main Wembanyama averages use full-workload games only, excluding early-exit games from the primary run averages while still keeping those games in the full game log. Several historical tables depend on Stathead, StatMuse, Basketball Reference, NBA.com, and PBPStats.com source context. Because the run is ongoing in the file, pending or unverified box-score rows should be updated before final publishing.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
