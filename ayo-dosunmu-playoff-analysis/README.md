# Ayo Dosunmu and the Value of Playoff Stability

## Overview

This project evaluates Ayo Dosunmu’s playoff role with Minnesota and why his offensive stability has mattered during a high-leverage series against Denver. The analysis looks at his post-trade fit, playoff scoring jump, shot profile, playtype translation, and Game 4 breakout. It frames Dosunmu as a bench guard whose value comes from fitting around stars while still giving Minnesota enough creation when the offense needs it.

## Main Question

Has Ayo Dosunmu’s playoff scoring burst been supported by real role growth, or is it mostly short-sample shooting variance?

## Data Sources

- NBA.com
- Databallr
- Synergy / SportsRadar
- Basketball Reference

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- readxl
- janitor
- kableExtra
- showtext / sysfonts
- forcats

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Pre-trade vs. post-trade production table
- Regular season vs. postseason role table
- Role expansion chart
- Efficiency comparison chart
- Shot profile by zone table
- Playtype efficiency chart
- Playtype usage chart
- Core playtype comparison table
- Playoff PnR leaderboard table
- Playoff three-point leaderboard table
- Historic 40-point bench scoring tables

## Notes and Limitations

This project uses local Excel files for playtype, shot-zone, and leaderboard inputs. Some of those inputs appear to come from proprietary or restricted sources such as Synergy/SportsRadar and should not be included in a public GitHub repository unless you have permission. The cleaned R Markdown file is structured to read those files locally through YAML parameters, so the report will not fully render without those files or anonymized/sample replacements. The playoff sample is also small, so three-point shooting, true shooting, and pick-and-roll efficiency should be interpreted with regression risk in mind.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
