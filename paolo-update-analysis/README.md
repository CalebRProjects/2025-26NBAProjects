# Is Paolo Banchero Starting to Figure It Out?

## Overview

This project revisits Paolo Banchero’s 2025-26 season after a more encouraging recent stretch, focusing on whether his improved production reflects a real shift in offensive approach.

Earlier in the season, the concern was not Banchero’s talent. It was the way his offense was being generated. Too many possessions ended in difficult self-created jumpers, limiting the value of his size, strength, foul drawing, and downhill playmaking.

This follow-up looks at whether that profile is starting to change. Over his last 10 games, Banchero has produced efficiently while Orlando has won games, but the key question is whether the improvement is supported by better shot selection, more rim pressure, and stronger lineup context rather than temporary shot-making.

## Main Question

Is Paolo Banchero’s recent improvement a meaningful role shift, or just a short-term hot stretch?

## Methodology

This analysis compares Banchero’s early-season profile to his recent form using shot data, free-throw rate, play-type efficiency, lineup context, and on/off indicators.

The recent-form section uses manually entered last-10-game summary numbers for Banchero’s scoring, rebounding, assists, shooting efficiency, and Orlando’s record during that stretch.

The shot-profile section uses NBA.com ShotChartDetail data pulled through `hoopR`. Shots are cached locally, then split into two samples:

- Games 1-45
- Last 10 games

Each shot is classified into one of three broad areas:

- Rim
- Midrange
- 3PT

The goal is to see whether Banchero’s recent efficiency is being supported by a better shot mix. A more sustainable version of his offense should feature more rim pressure, fewer difficult midrange attempts, and a free-throw rate that reflects his physical advantage.

Free-throw rate is calculated as:

`FTA / FGA`

The report also compares Banchero’s free-throw rate to other high-scoring players across the league to show how unusual his foul pressure is.

The play-type section uses Synergy/SportsRadar data. Season-level play-type data is loaded from an Excel file, while the first-25-game sample is entered manually. Games 26-56 are estimated by subtracting the first-25-game totals from the current season totals. This allows the report to compare whether Banchero’s key play types have become more efficient as the season has progressed.

The lineup section uses manually entered DataBallr lineup data for selected Banchero three-man combinations. These lineups are used to identify which roster contexts are helping him most, especially combinations that provide spacing, defensive stability, or both.

The overall impact section uses a manually entered DataBallr-style on/off snapshot to summarize whether Banchero’s individual production is translating into team-level impact.

## Data Sources

- NBA.com
- hoopR
- Synergy/SportsRadar
- DataBallr

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- readxl
- readr
- kableExtra / knitr
- forcats
- lubridate
- showtext / sysfonts
- scales
- stringr

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Recent-form table
- Shot mix chart comparing Games 1-45 vs Last 10
- Shot profile table comparing Games 1-45 vs Last 10
- Free-throw rate league-context table
- Full play-type efficiency table
- Play-type efficiency comparison chart
- Selected Banchero three-man lineup table
- Overall lineup impact snapshot table
- Impact interpretation table
- Final takeaway on Banchero’s recent offensive shift

## Notes and Limitations

This report is a follow-up analysis, not a full season model. It is focused on whether Banchero’s recent stretch shows evidence of a more sustainable offensive approach.

The shot-profile comparison uses broad location groups rather than detailed shot-quality modeling. A rim, midrange, or three-point classification helps show shot mix, but it does not fully capture defender distance, clock context, play design, or whether the shot was self-created.

The play-type comparison estimates Games 26-56 by subtracting manually entered first-25-game values from current season totals. This is useful for directional comparison, but it depends on the season-level Synergy/SportsRadar file and the first-25-game manual inputs being aligned.

The lineup and impact tables are manually entered snapshots from DataBallr-style research. They should be refreshed before final publishing if the underlying data changes.

The last-10-game sample is small. Banchero’s recent numbers are encouraging, especially the improved efficiency and free-throw pressure, but the analysis should be read as an early signal rather than proof that his shot-selection issues are solved.

The purpose of the project is not to reverse the earlier concerns completely. It is to identify whether Banchero is beginning to lean more heavily into the parts of his game that can make Orlando’s offense more sustainable.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
