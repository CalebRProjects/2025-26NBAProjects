# LA Clippers Bounceback: What Broke, What Changed, What It Means

## Overview

This project evaluates the LA Clippers’ midseason turnaround after a 6–21 start. The analysis focuses on whether the bounceback came from a real shift in offensive execution or simply short-term shotmaking. It uses team game logs, rolling offensive trends, league context, and Harden/Leonard lineup splits to explain what changed and what the turnaround means.

## Main Question

Did the Clippers’ bounceback come from a meaningful improvement in offensive process, or were they mostly benefiting from temporary shooting variance?

## Data Sources

- NBA.com
- hoopR / NBA Stats API
- Databallr

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- janitor
- kableExtra
- lubridate
- showtext / sysfonts

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Start vs. since-split offensive profile table
- Rolling offensive trends chart
- Last-17 league context table
- Top-10 offenses table
- Shot profile comparison chart
- Harden/Leonard season stats table
- Harden/Leonard WOWY lineup split table

## Notes and Limitations

This report depends on live NBA Stats API pulls through `hoopR`, with optional CSV caching controlled by the YAML parameters. If NBA.com changes endpoint schemas, blocks requests, or rate-limits calls, the report may require cached files or small code adjustments. Possession estimates use a standard box-score approximation, so offensive rating and related rates may differ slightly from official NBA.com values. Harden/Leonard counting stats and WOWY splits are manually entered, so the report is not fully reproducible from raw public data alone.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
