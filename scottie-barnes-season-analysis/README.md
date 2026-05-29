# Scottie Barnes: Two-Way Engine + DPOY Case

## Overview

This project evaluates Scottie Barnes’ 2025-26 season as a two-way engine and Defensive Player of the Year candidate.

The analysis focuses on how Barnes’ offensive role and defensive impact have shifted within Toronto’s roster context. Offensively, Barnes remains most valuable as a passing hub, transition threat, connective scorer, and advantage creator. Defensively, he has emerged as one of the league’s most disruptive and versatile players, combining on-ball workload, event creation, weakside rim protection, and defensive rebounding.

The central argument is that Barnes’ value is not built around elite scoring volume. It comes from the way his size, feel, activity, and versatility connect both sides of the floor. With Brandon Ingram absorbing more offensive responsibility, Barnes has been able to lean harder into the defensive role that has powered Toronto’s rise into one of the league’s best defensive teams.

## Main Question

Does Scottie Barnes’ two-way impact, especially his defensive workload and event creation, support a legitimate Defensive Player of the Year case?

## Methodology

This analysis uses NBA.com data exported into Excel workbooks, then cleaned and analyzed in R.

The Excel files referenced in the R Markdown file are not included in the public repo. They were created by exporting or manually saving NBA.com tables, then reading them into R with `readxl`. In the cleaned version of the file, those inputs are expected to live in:

- `data/raw/`

The counting-stats section uses season-level NBA.com player data from 2021-22 through 2025-26. It highlights Barnes’ current season career highs to show where his offensive profile has improved, especially in efficiency, passing, and all-around production.

The playtype section uses NBA.com playtype data exported to Excel. It summarizes Barnes’ offensive possessions, percent time, points per possession, shooting efficiency, turnover rate, and two-point/three-point splits by play type. This helps identify where his size and passing create advantages.

The shot-zone section uses NBA.com shot-zone data over the last three seasons. The report compares Barnes’ field-goal percentage by zone and plots those trends against his three-season average in each area. This shows where his scoring has stabilized and where the jumper remains inconsistent.

The defensive event-creation section uses NBA.com leaderboards for stocks and deflections. These tables show Barnes’ activity level compared with the rest of the league and help support the argument that his defensive value comes from disruption, not just positional versatility.

The on-ball defense section uses NBA.com defensive playtype data exported to Excel. It evaluates Barnes’ defensive workload and results across possession types, including isolation and pick-and-roll ball-handler defense. The code is designed to handle slight differences in exported column names.

The team-impact section uses an NBA.com team defensive rating leaderboard exported to Excel. This places Toronto’s defensive performance in league context and connects Barnes’ individual defensive role to the Raptors’ team-level defensive identity.

## Data Sources

- NBA.com

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- readxl
- janitor
- kableExtra / knitr
- lubridate
- showtext / sysfonts
- scales
- stringr

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Barnes counting-stats table with 2025-26 career highs highlighted
- Offensive playtype table
- Shot-zone table for the last three seasons
- Shot-zone FG% trend chart
- Stocks per game leaderboard
- Deflections per game leaderboard
- Defensive playtype workload and efficiency table
- Team defensive rating leaderboard
- DPOY case and conclusion section

## Notes and Limitations

This report is built from NBA.com tables that were exported into Excel files. Those Excel files are not included in the public repo by default. To rerender the report, the same source files should be placed in `data/raw/` or the file paths in the R Markdown parameters should be updated.

Because the workbook inputs are exported snapshots, the analysis is not fully reproducible from public API calls alone. If the underlying NBA.com leaderboards or playtype tables change, the Excel exports should be refreshed before final publishing.

The offensive and defensive playtype sections are descriptive. They show how Barnes performs across different possession types, but they do not fully capture matchup difficulty, opponent quality, scheme responsibility, lineup context, or late-clock burden.

The defensive event-creation metrics, including stocks and deflections, are useful indicators of activity and disruption. They should not be treated as complete defensive value metrics on their own.

The DPOY framing is focused on Barnes’ case. It uses Toronto’s team defense, Barnes’ workload, and his event creation to build the argument, but it is not a complete award model comparing every candidate with identical criteria.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
