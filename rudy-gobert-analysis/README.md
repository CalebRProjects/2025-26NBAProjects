# Rudy Gobert: Defensive Impact & DPOY Case

## Overview

This project evaluates Rudy Gobert’s 2025-26 Defensive Player of the Year case through defensive on/off impact, rim protection, defensive field-goal suppression, and finishing value.

The analysis focuses on Gobert as the backbone of Minnesota’s defensive structure. When he is on the floor, the Timberwolves defend at a high level; when he sits, the defense falls apart. The project also includes an offensive finishing section to show that while Gobert’s offensive role remains limited, he is still producing meaningful value through rim finishing, offensive rebounding, and screen assists.

The main argument is that Gobert’s impact may not look as flashy as newer defensive stars, but the team-level dependence on his rim protection and defensive infrastructure remains massive.

## Main Question

Does Rudy Gobert’s defensive impact in 2025-26 still support a legitimate Defensive Player of the Year case?

## Data Sources

- NBA.com
- Databallr

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- bookdown
- readxl
- janitor
- knitr
- forcats
- stringr
- scales

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Career finishing by zone table
- Restricted-area and paint finishing trend chart
- Minnesota on/off table with Gobert on versus off
- Total defensive field-goal impact leaderboard
- Rim-only defensive field-goal impact leaderboard
- Total DFG% impact bar chart
- Rim protection impact bar chart
- Opponent rim FG% ranking chart
- DPOY case and conclusion section

## Notes and Limitations

This report combines manually entered defensive tracking leaderboards with an external Excel file containing Gobert’s career rim and paint finishing data. The cleaned R Markdown file expects the finishing file to be stored at `data/raw/RudyGobert_RimAndPaintFinishing_Career.xlsx`, unless the `finishing_path` parameter is changed.

The defensive field-goal impact tables are manually entered snapshots from NBA.com tracking data. They should be refreshed before final publishing if the leaderboard changes or if the season is still active.

The on/off ratings are also entered directly rather than recalculated from possession-level play-by-play data. They are used as verified team-context indicators, not as a fully reproducible possession model.

Defensive field-goal percentage impact is useful, but it does not capture every part of defense. Matchup difficulty, scheme responsibility, deterrence, positioning, and teammate context all affect how these numbers should be interpreted.

The DPOY framing is intentionally focused on Gobert’s case. It compares his impact to other elite defenders where relevant, but it is not a complete award model.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
