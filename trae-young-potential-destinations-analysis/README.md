# Trae Young Trade Fit Analysis

## Overview

This project evaluates Trae Young’s offensive impact, roster fit, and structural tradeoffs through two possible trade destinations: the Washington Wizards and Toronto Raptors. The analysis was built while Young trade rumors were still uncertain, with both versions prepared as scenario-based reports.

Young ultimately landed with Washington, so the Wizards version became the primary report. The Raptors version remains included as a contingency analysis showing how his fit would have looked in a different team context.

The project focuses on Young less as a raw producer and more as a roster-construction problem. His pick-and-roll creation, passing, offensive load, and free-throw pressure can organize a functional offense, but his defensive limitations and ball-dominant style require specific support: spacing, rim protection, point-of-attack defense, and secondary creation.

## Main Question

How does Trae Young’s offensive value translate to a new roster, and what team context is best equipped to support the strengths and weaknesses of a Trae-led offense?

## Data Sources

- NBA.com
- Synergy/SportsRadar
- CraftedNBA
hoopR

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- kableExtra
- hoopR
- showtext / sysfonts

## Key Outputs

- Knitted PDF report for Washington fit
- Knitted PDF report for Toronto fit
- Cleaned R Markdown analysis file, if included
- Trae Young career offensive profile table
- Usage rate and true shooting trend chart
- Shot-type efficiency table
- Shot-type field-goal percentage trend chart
- Play-type portfolio table
- Pick-and-roll ball handler trend chart
- Play-type usage distribution chart
- On/off impact table
- CraftedNBA offensive metrics table
- CraftedNBA defensive metrics table
- Washington roster-fit analysis
- Toronto roster-fit contingency analysis

## Notes and Limitations

This project was built as a scenario-based trade analysis, not a fully automated projection model. The Wizards version should be treated as the primary report because Washington became the final destination. The Raptors version was created before the trade outcome was known and is included as a contingency report.

Some portions of the analysis depend on manually gathered or summarized source tables, including Synergy/SportsRadar play-type data and CraftedNBA percentile profiles. Because the reports were created in response to active trade rumors, final roster details, team context, or post-trade usage patterns should be updated before treating the findings as a long-term projection.

The analysis is primarily focused on fit, role, and roster structure. It does not attempt to fully simulate team wins, lineup performance, or future player development outcomes.

## File Guide

- `trae-young-wizards-analysis.pdf` — primary rendered report evaluating Trae Young’s fit with Washington
- `trae-young-raptors-analysis.pdf` — contingency rendered report evaluating Trae Young’s potential fit with Toronto
- `trae-young-wizards-analysis.rmd` — source analysis file
- `trae-young-raptors-analysis.rmd` — source analysis file
- `README.md` — project overview

