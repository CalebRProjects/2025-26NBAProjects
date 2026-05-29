# The Hornets Are BUZZING

## Overview

This project examines the Charlotte Hornets’ late-season surge during the 2025-26 season, focusing on whether their recent improvement reflects a real team-level shift or a temporary hot stretch.

The analysis compares Charlotte’s full-season profile to its last 16 games, then places that recent form in league context. The Hornets’ offense had already shown upside, but the larger change has come from improved defense, better rebounding, stronger lineup balance, and clearer roles under Charles Lee.

The project focuses on how Charlotte turned a near-.500 baseline into one of the league’s strongest recent stretches, with attention to Brandon Miller’s scoring jump, LaMelo Ball’s offensive organization, Kon Knueppel’s efficiency, Moussa Diabaté’s interior impact, and the young core’s lineup fit.

## Main Question

Are the Hornets actually better lately, and what has changed during their recent surge?

## Data Sources

- NBA.com
- hoopR
- DataBallr

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- kableExtra / knitr
- janitor
- lubridate
- showtext / sysfonts
- scales
- readr
- stringr
- purrr

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Season versus recent-form summary table
- Season-long point differential chart
- Rolling 10-game margin chart
- Last-16 league-context table
- Top offenses over the last 16 games table
- Top defenses over the last 16 games table
- Top net ratings over the last 16 games table
- Process versus results table
- Season versus last-16 box-score delta table
- Player overview table for Charlotte’s core contributors
- Brandon Miller, LaMelo Ball, Kon Knueppel, Miles Bridges, and Moussa Diabaté player sections
- Core WOWY lineup split table
- Coaching, bench, and final takeaways section

## Notes and Limitations

This report combines NBA.com data pulled through `hoopR` with cached team and player dashboard data. The cache structure is used to make the report easier to rerun without repeatedly pulling the same endpoints.

The last-16-game window is descriptive rather than predictive. Charlotte’s recent net rating, offensive efficiency, and defensive improvement show how well the team has played during this stretch, but they do not guarantee that the same level will hold over a larger sample.

Some lineup-context numbers come from WOWY-style splits and should be interpreted with sample size caution. Even 200-minute lineup samples can shift quickly because of shooting variance, opponent strength, and rotation changes.

The analysis is focused on identifying what changed during the surge, not building a full playoff projection or long-term team forecast. Because the report was built during an active season, player averages, league rankings, team records, and lineup samples should be refreshed before final publishing.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
