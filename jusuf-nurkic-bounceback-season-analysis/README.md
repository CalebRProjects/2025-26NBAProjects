# Jusuf Nurkić: The Bosnian Beast Has Bounced Back

## Overview

This project evaluates Jusuf Nurkić’s 2025-26 bounce-back season with Utah, focusing on how he has reestablished value as a reliable veteran center.

The analysis looks beyond scoring and focuses on the parts of Nurkić’s game that still translate: rebounding, passing, screening, interior finishing, defensive positioning, and activity. He is no longer being asked to carry offense or anchor an elite defense, but his feel, size, and connective skill set have made him useful again in a more defined role.

The project frames Nurkić as a dependable center option whose value comes from stabilizing possessions, ending defensive trips, creating advantages through passing and screens, and holding up well enough defensively within the right team context.

## Main Question

Has Jusuf Nurkić genuinely bounced back as a useful NBA center, and what parts of his game are driving that value?

## Methodology

This analysis combines season-to-date NBA dashboard data, player game logs, manually entered leaderboard snapshots, and shot-profile information to evaluate Nurkić’s role from multiple angles.

The baseline section pulls Nurkić’s season-level production through the selected `as_of_date`, including minutes, scoring, rebounding, assists, turnovers, field-goal efficiency, three-point attempts, and free-throw percentage.

The rebounding section uses player game logs and advanced dashboard rebounding rates to evaluate both single-game rebounding peaks and season-long rebounding value. The top rebounding games table highlights his best individual performances, while the rebounding-rate section places his value in a more stable possession-based context.

The rebounding/playmaking quadrant compares players by assists per game and rebounds per game, filtering for players with at least 30 games played and 20 minutes per game. The dashed reference lines represent the 75th percentile in each category, helping show which players combine above-average rebounding and passing volume.

The playmaking section uses recent game logs to show his last-10 assist trend, while the screen-assist table adds off-ball offensive value that does not show up in normal box-score creation.

The scoring section uses NBA.com shot-location dashboard data to summarize where Nurkić’s attempts come from: restricted area, non-restricted-area paint, midrange, and three-point range. This keeps the scoring analysis focused on role and shot diet rather than treating him like a high-usage scorer.

The defensive section uses manually entered rim-protection and deflection leaderboards. These are used to evaluate whether Nurkić still holds up as a functional interior defender and whether his anticipation/activity compensate for reduced mobility.

## Data Sources

- NBA.com
- hoopR

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- janitor
- kableExtra / knitr
- readr
- readxl
- lubridate
- ggrepel
- ggimage
- magick
- showtext / sysfonts
- scales
- glue

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Season baseline table
- Top 5 rebounding games table
- Rebounding rate profile table
- Rebounding plus playmaking quadrant chart
- Last-10-game assist trend chart
- Screen assists per game table
- Shot profile by NBA.com zone table
- Shot diet by zone chart
- Rim-defense company table
- Deflections among centers table
- Closing bounce-back summary

## Notes and Limitations

This report combines live NBA.com data pulled through `hoopR` with manually entered leaderboard snapshots. The live pulls are cached locally to make the report easier to rerun, while the manually entered tables should be refreshed before final publishing if the underlying leaderboards change.

The analysis is built around a selected `as_of_date`, so the results represent Nurkić’s season profile only through that date. Updating the date or rerunning later in the season may change his averages, rankings, and recent-trend sections.

The rebounding/playmaking quadrant uses per-game assists and rebounds with games-played and minutes filters. This is useful for role context, but it does not adjust for pace, usage, teammate shot quality, or passing role.

The shot-profile section uses broad NBA.com dashboard zones, not possession-level shot quality. It shows where Nurkić is taking shots and how often, but it does not fully explain how those shots were created.

The defensive section should be interpreted carefully. Rim defensive field-goal differential and deflections are useful indicators, but they do not fully capture scheme responsibility, matchup difficulty, deterrence, positioning, or team defensive context.

The purpose of the project is not to argue that Nurkić has returned to his peak Portland form. It is to show that he has bounced back into a useful, clearly defined center role.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
