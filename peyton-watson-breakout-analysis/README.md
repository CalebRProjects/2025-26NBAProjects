# Peyton Watson: Jump-Shooting Leap + Defensive Value

## Overview

This project evaluates Peyton Watson’s 2025-26 breakout through his jump-shooting growth, shot-profile development, defensive value, and changing role within the Denver Nuggets.

The analysis focuses on how Watson has moved from long-term developmental wing to clear positive rotation player. His leap is not just about more minutes or more shots. The key change is that he is converting better from the same areas of the floor, especially as a catch-and-shoot threat, while still providing the defensive versatility that made him valuable early in his career.

The project also examines Watson’s expanded role during Denver’s stretch without Nikola Jokić, where he handled more offensive responsibility without losing efficiency. That stretch helps frame the larger roster question: Watson’s development is a major win for Denver, but it also creates a difficult financial decision as he approaches restricted free agency.

## Main Question

Has Peyton Watson’s offensive leap made him a long-term core piece for Denver, and how should his improved shooting be weighed alongside his defensive value and upcoming contract situation?

## Methodology

This analysis uses NBA.com data exported into Excel workbooks, then cleaned and analyzed in R.

The Excel files are included in the repo’s `data/` folder and are used as local inputs for the R Markdown file.

The catch-and-shoot section uses NBA.com/Synergy-style catch-and-shoot data exported to Excel. It compares Watson’s guarded and unguarded catch-and-shoot efficiency across seasons, focusing on time share, field-goal percentage, effective field-goal percentage, makes, and attempts.

The shot-distance section uses NBA.com shot-location data exported to Excel. It breaks Watson’s attempts into several distance zones:

- Restricted area
- Non-restricted-area paint
- Midrange
- Corner threes
- Above-the-break threes

The report calculates each zone’s share of Watson’s total shot profile by season, then tracks efficiency trends over time. This helps separate shot-selection changes from actual shot-making improvement.

The zone-efficiency section compares Watson’s current efficiency to his career baseline in key scoring areas. The goal is to determine whether the offensive leap is mostly coming from a new shot diet or from improved conversion in the same offensive role.

The defensive section uses NBA.com closest-defender data exported to Excel. It evaluates Watson’s defensive field-goal impact within six feet, including defended field-goal attempts, opponent field-goal percentage, expected field-goal percentage, and differential. Negative differential means opponents shot worse than expected when Watson was the closest defender.

The no-Jokić context section uses a manually summarized stretch to explain how Watson’s role expanded when Denver’s offensive ecosystem changed. That section is used as context for role scalability, not as a full replacement for season-long data.

## Data Sources

- NBA.com
- Manually summarized no-Jokić stretch context

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- bookdown
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
- Catch-and-shoot summary table
- Catch-and-shoot eFG% trend chart
- Shot-volume share by distance chart
- Zone field-goal percentage trend chart
- Defensive shot-impact table
- Defensive field-goal impact trend chart
- No-Jokić context section
- Denver offseason decision section
- Limitations section

## Notes and Limitations

This report is built from NBA.com tables exported into Excel files. Those files are included in the repo’s `data/` folder and are required to rerender the report unless the paths in the R Markdown parameters are changed.

Because the workbook inputs are exported snapshots, the analysis should be refreshed before final publishing if the NBA.com tables change or if the season is still active.

Catch-and-shoot and shot-zone splits can be sensitive to sample size, especially when broken down by contest level or specific court zone. The results should be read as directional evidence of shooting development rather than a final estimate of Watson’s true shooting talent.

Defensive field-goal impact is useful, but it does not fully capture individual defense. Matchup difficulty, scheme responsibility, teammate help, role changes, and Denver’s overall defensive context all affect how Watson’s numbers should be interpreted.

The no-Jokić stretch is included because it is important context for Watson’s expanded role, but it is still a small sample. It should be treated as evidence that his game may scale upward, not proof that his higher-usage role is guaranteed to hold long term.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
- `data/Peyton Watson C&S Data.xlsx` — NBA.com catch-and-shoot export
- `data/Peyton Watson Shot Distance Data.xlsx` — NBA.com shot-distance export
- `data/Peyton Watson Defense Data.xlsx` — NBA.com closest-defender export
