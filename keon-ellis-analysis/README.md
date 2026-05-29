# Keon Ellis: Why Teams Want Him

## Overview

This project evaluates Keon Ellis as a low-usage, high-impact rotation guard whose value comes from defensive event creation, offensive efficiency, and lineup scalability rather than traditional box-score production.

The analysis focuses on why Ellis is appealing to teams even without high scoring volume. Offensively, he plays within a narrow and efficient role: spot-up shooting, rim attempts, transition chances, and low-turnover connective play. Defensively, he creates events at an elite rate, ranking near the top of the league in steal-based metrics, deflections, and stop creation.

The project frames Ellis as the kind of role player whose value grows next to high-level offensive talent because he can defend, finish plays, avoid mistakes, and stay out of the way offensively.

## Main Question

Why do teams value Keon Ellis, and does his impact profile support viewing him as a scalable, playoff-viable rotation guard?

## Data Sources

* NBA.com
* hoopR
* Cleaning the Glass
* DataBallr

## Tools Used

* R
* tidyverse
* ggplot2
* R Markdown
* hoopR
* knitr
* ggrepel
* showtext / sysfonts
* lubridate
* scales

## Key Outputs

* Knitted PDF report
* Cleaned R Markdown analysis file
* Cleaning the Glass on/off impact table
* DataBallr impact and event-creation table
* Usage versus efficiency scatterplots
* 2024-25 Keon Ellis shot chart
* 2025-26 Keon Ellis shot chart
* Efficiency by context table
* Steals per 36 distribution charts
* Steal percentage distribution charts
* Defensive playmaking and event-creation section
* Team fit and market value section

## Notes and Limitations

This report combines live NBA.com data pulled through `hoopR` with manually entered Cleaning the Glass and DataBallr impact metrics. The manually entered sections preserve specific source snapshots from the time of analysis and should be refreshed before final publishing if updated numbers are available.

The 2025-26 sample is smaller than the 2024-25 sample, so early-season dips in shooting efficiency or on/off impact should be treated cautiously. The report emphasizes multi-season patterns rather than overreacting to a partial-season sample.

The defensive event-creation charts use NBA.com defensive tracking and box-score-derived rates. Those numbers help identify activity and disruption, but they do not fully capture matchup difficulty, scheme responsibility, or off-ball positioning.

The analysis is focused on player archetype, market value, and team fit. It does not attempt to estimate trade value, contract value, or exact playoff lineup impact.

## File Guide

* `analysis.Rmd` — source analysis file
* `analysis.pdf` — rendered report
* `README.md` — project overview
