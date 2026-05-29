# Rebound After Slow Starts

## Overview

This project identifies NBA players who improved after poor early-season results during the 2025-26 season.

The analysis compares each player’s first half of games played to their most recent half of games played, then evaluates whether the improvement looks more like a role change, shot-diet change, or natural shot-making regression. The project focuses on players whose production, efficiency, or offensive profile shifted meaningfully over the course of the season.

The goal is not just to show that a player improved. The goal is to diagnose why the improvement happened and whether it appears tied to more sustainable indicators like minutes, usage, free-throw rate, three-point volume, or shot-zone efficiency.

## Main Question

Which players rebounded after slow starts, and were those improvements driven by role changes, shot-profile changes, or better shot-making?

## Data Sources

- NBA.com
- hoopR

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- ggrepel
- kableExtra / knitr
- showtext / sysfonts
- lubridate
- scales

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Early versus recent half-season split table
- Core rebound signals table
- Shot-diet and role-proxy table
- True shooting versus three-point attempt share chart
- Player-by-player diagnostic pages
- Player first-half versus second-half tables
- Player change-summary tables
- Shot-zone efficiency heatmaps
- Cached shot chart data by player

## Notes and Limitations

This report uses player-specific first-half versus recent-half splits based on games played, not fixed calendar windows. That means each player’s “early” and “recent” sample depends on their own game log rather than a shared leaguewide cutoff date.

The diagnostics are descriptive rather than causal. A rise in true shooting percentage, three-point attempt share, or free-throw rate can suggest a role or shot-profile change, but it does not prove why the improvement occurred. Injury status, opponent strength, lineup context, and team strategy are not fully modeled.

Shot-zone data is pulled and cached separately from game logs. Because the project uses cached files, the shot data should be refreshed before final publishing if the season is still active or if the analysis is being rerun later.

The analysis is designed to flag interesting rebound cases, not produce a definitive ranking of improved players.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
