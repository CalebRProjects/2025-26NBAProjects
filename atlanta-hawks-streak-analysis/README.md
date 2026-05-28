# An 11-Game Run, and a Reality Check

## Overview

This project evaluates the Atlanta Hawks’ 11-game winning streak during the 2025–26 season. The analysis looks at how the run compares historically, where Atlanta stood in the Eastern Conference before and after the streak, and whether the process behind the run looks sustainable. It focuses on the balance between real improvement, especially defensively, and likely regression from hot shooting and short-sample performance.

## Main Question

How real was Atlanta’s 11-game run, and which parts of the streak looked sustainable versus likely to cool off?

## Data Sources

- NBA.com
- hoopR / NBA Stats API
- StatMuse
- Manually entered franchise streak history
- Manually entered standings context
- Manually entered remaining strength-of-schedule table

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- janitor
- kableExtra
- showtext / sysfonts
- lubridate
- stringi

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Hawks franchise streak history table
- Longest winning streaks this season table
- Eastern Conference standings shift table
- Season vs. recent form table
- Point differential trend chart
- Last-11 league context table
- Top-10 offense, defense, and net rating tables
- Process vs. results table
- Core player production table
- Remaining strength-of-schedule table

## Notes and Limitations

This report depends on live NBA Stats API pulls through `hoopR`, with optional local caching enabled through the YAML parameters. If NBA.com changes its schema, blocks requests, or rate-limits calls, the report may require cached `.rds` files or small code adjustments. Some context tables are manually entered, including franchise streak history, standings movement, and remaining strength of schedule. The analysis is also centered on an 11-game sample, so shooting efficiency, defensive rating, and individual hot stretches should be interpreted with regression risk in mind.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
- `hawks_run/cache/` — optional local cache for NBA API pulls
- `data/` — local or excluded data files, if applicable
