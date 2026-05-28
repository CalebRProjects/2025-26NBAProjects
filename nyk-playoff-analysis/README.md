# The Knicks' Best Finals Path in Years

## Overview

This project evaluates the New York Knicks’ 2026 playoff run through team-level performance, star production, role-player swings, lineup context, and Net Points impact. The analysis focuses on why this Knicks roster looks more complete than prior playoff versions, especially with Brunson, Towns, and Anunoby sharing the burden more cleanly. It also highlights the remaining questions around OG’s health, Mikal Bridges’ consistency, and Josh Hart’s offensive limitations.

## Main Question

Do the Knicks have a real Finals path, and is their current playoff formula balanced enough to hold up beyond the East?

## Data Sources

- NBA.com
- Synergy / SportsRadar
- Databallr
- ESPN Analytics
- Manually entered playoff split and game-level data

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- kableExtra
- showtext / sysfonts
- forcats
- stringr

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Remaining playoff teams profile table
- Net Rating comparison chart
- Offensive and defensive balance scatterplot
- Player split comparison tables
- KAT playoff playtype shift chart
- Brunson playoff workload chart
- Mikal Bridges scoring progression chart
- Knicks Net Points table and bar chart
- Cumulative playoff margin chart

## Notes and Limitations

This analysis uses manually entered data from public and user-collected sources, so the file is not fully automated from raw data. Some source data, especially Synergy/SportsRadar, Databallr, and ESPN Analytics Net Points, may be proprietary or restricted. Small playoff samples should be interpreted carefully, especially for shooting efficiency, playtype shifts, and lineup splits. OG Anunoby’s availability is a major contextual factor, and the analysis reflects the roster situation at the time the report was written.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
