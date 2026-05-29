# Gui Santos Was Not Supposed to Matter This Much

## Overview

This project examines Gui Santos’s unexpected emergence as a meaningful contributor for Golden State during a difficult stretch of the 2025-26 season.

With Steph Curry and Jimmy Butler unavailable, Santos was forced into a larger role under poor team conditions. Rather than simply increasing volume, he showed real growth: improving efficiency, expanding his offensive responsibilities, and developing into a more complete player.

The analysis focuses on how Santos’s production scaled with opportunity, how his shot profile changed, and whether his offensive and defensive growth suggests he can hold a long-term rotation role.

## Main Question

Did Gui Santos’s production increase simply because of more minutes, or did his expanded role reveal real development that could translate to a competitive rotation?

## Data Sources

- NBA.com
- hoopR
- Databallr

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- kableExtra
- ggrepel
- showtext / sysfonts
- janitor
- lubridate

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Warriors with and without Steph/Jimmy table
- Gui Santos top games without Steph and Jimmy table
- Raw stats split table
- Per-75 possession split table
- Shot diet and foul pressure table
- Shot distribution by zone chart
- Usage rate versus true shooting league-context chart
- Defensive growth trend chart
- Three-year player development snapshot table

## Notes and Limitations

This report combines live NBA.com data pulled through `hoopR` with manually entered split tables and Databallr-derived player impact context. The manually entered sections are used to preserve the version of the analysis that was written at the time.

The Warriors team-context split is based on whether Steph Curry and Jimmy Butler appeared in the game logs. Any injury-report nuance, minutes restrictions, or partial-game exits are not fully modeled.

The role-shift tables use fixed early-versus-late season splits rather than a rolling model or possession-level role classifier. The analysis should therefore be treated as descriptive player evaluation rather than a causal proof of development.

Because the file was built during an active season, player averages, team context, and league rankings should be refreshed before final publishing.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
