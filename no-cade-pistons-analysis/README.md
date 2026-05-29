# 9–3 Without Cade, and Finally Built to Survive It

## Overview

This project examines how the Detroit Pistons have performed without Cade Cunningham during the 2025-26 season.

Detroit’s rise still starts with Cade. He is the offensive engine and the player who gives the team its clearest structure. But the Pistons’ success without him has become one of the more important signals of how far the roster has come. In previous years, Cade’s absence would have exposed how fragile the team was. This season, Detroit has survived those stretches by leaning into defense, rebounding, size, and collective playmaking.

The analysis focuses on the Pistons’ 9–3 record without Cade, including how the team profile changes when he is out, which players have carried the largest burden, and what this says about Detroit’s growth entering the playoffs.

## Main Question

Are the Pistons finally built well enough to survive without Cade Cunningham, and what does their Cade-out success reveal about the team’s broader playoff readiness?

## Data Sources

NBA.com
hoopR

## Tools Used

R
tidyverse
ggplot2
R Markdown
knitr
hoopR
janitor
showtext / sysfonts
lubridate

## Key Outputs

Knitted PDF report
Cleaned R Markdown analysis file
Team profile with Cade in versus out table
Points scored versus points allowed chart
Style-shift chart comparing rebounds, assists, and turnovers
Teammate production table in Cade-out games
Jalen Duren top Cade-out games table
Daniss Jenkins top Cade-out games table
Best supporting-player Cade-out games table
Manual game-log add-ins for missing or incomplete pulled games

## Notes and Limitations

This report combines NBA.com data pulled through `hoopR` with manual add-ins for games that were not fully represented in the pulled logs at the time of analysis. The manual entries are used to preserve the Cade-out sample and keep the analysis aligned with the actual game results.

The team profile uses verified offensive and defensive ratings for the Cade-in and Cade-out splits rather than fully recalculating those ratings from raw possession-level data inside the file. Possession-style metrics and box-score summaries should therefore be treated as descriptive context, not a complete play-by-play model.

The Cade-out sample is still small. Detroit’s success without Cade does not change the fact that he is the foundation of the team. The purpose of the analysis is to show that the roster no longer collapses when he is unavailable, not to argue that the Pistons are better without him.

Because the report was built during an active season, player averages, team record, injury context, and late-season standings should be updated before final publishing.

## File Guide

`analysis.Rmd` — source analysis file
`analysis.pdf` — rendered report
`README.md` — project overview
