# Neemias Queta: Flourishing in TD Garden

## Overview

This project evaluates Neemias Queta’s expanded role with the Boston Celtics during the 2025–26 season. The analysis looks at whether his production and impact hold up under starter-level responsibility, with emphasis on play finishing, rim pressure, rim protection, and on/off defensive value. The goal is to understand whether Queta has become a dependable frontcourt piece rather than a temporary solution.

## Main Question

Has Neemias Queta’s larger role with Boston translated into stable, winning impact on both ends of the floor?

## Data Sources

- NBA.com
- Synergy / SportsRadar
- DataBallr
- CraftedNBA
- hoopR / NBA Stats API

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- kableExtra
- hoopR
- ggimage
- showtext / sysfonts
- ragg

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Offensive play type efficiency table
- Traditional and advanced production tables
- Databallr on/off table
- CraftedNBA defensive impact table
- Net rating vs. minutes chart
- Offensive PPP by play type chart
- Rim defense league-context visual
- Rebounding environment visual

## Notes and Limitations

This project uses a mix of manually entered data, public NBA.com data, Synergy/SportsRadar play type data, DataBallr on/off splits, and CraftedNBA metrics. Some data sources may be proprietary, restricted, or difficult to reproduce without access. The rim-defense visual depends on live NBA Stats API calls through `hoopR`, so rendering may fail if the endpoint is unavailable, rate-limited, or blocked. On/off results should be interpreted with lineup, matchup, and role context rather than treated as a clean causal estimate.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
- `fig-rim-def-context.png` — generated rim defense chart
