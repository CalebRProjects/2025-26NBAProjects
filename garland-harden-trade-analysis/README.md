# Darius Garland for James Harden: A Timeline Trade

## Overview

This project evaluates a theoretical Darius Garland-for-James Harden trade through the lens of team timelines, offensive role translation, durability, and playoff fit. The analysis compares the two guards across scoring profile, shot zones, pull-up shooting, assist creation, playtype portfolio, on/off impact, and Garland’s production without Donovan Mitchell. The central argument is that Cleveland would be buying short-term playoff reliability, while the Clippers would be buying a younger offensive ceiling.

## Main Question

Does a Garland-for-Harden swap make sense as a timeline trade, with Cleveland prioritizing immediate playoff stability and the Clippers prioritizing long-term offensive upside?

## Data Sources

- NBA.com
- Databallr
- Synergy / SportsRadar

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- readxl
- janitor
- kableExtra
- hoopR
- lubridate
- showtext / sysfonts

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- 2025–26 season overview table
- Shot-zone efficiency trend chart
- Shot-distribution chart
- Garland pull-up shooting context table
- Potential assists leaderboard table
- Playtype share comparison chart
- Playtype efficiency table
- Databallr on/off table
- Garland with/without Donovan Mitchell table
- Timeline-based trade verdict

## Notes and Limitations

This report uses local Excel files for shot-zone, playtype, and assist leaderboard inputs. Some of those files may come from proprietary or restricted sources such as Synergy/SportsRadar or Databallr, so they should not be included in a public GitHub repository unless you have permission. Several tables are manually entered or source extracts, so the analysis is not fully reproducible from raw public data alone. The trade evaluation is context-driven and should be read as a roster/timeline argument rather than a definitive prediction.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
- `data/` — local data files
- `Darius Garland Shot Zones (Last 3 Seasons).xlsx` — local source input
- `James Harden Shot Zones (Last 3 Seasons).xlsx` — local source input
