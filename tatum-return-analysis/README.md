# Jayson Tatum’s Return Opened the Celtics’ Next Chapter

## Overview

This project evaluates Jayson Tatum’s return from an Achilles rupture during the 2025–26 season. The analysis focuses on how his scoring took time to return, while his rebounding, playmaking, lineup impact, and overall control of the game translated almost immediately. By the playoffs, the scoring started to catch up, reinforcing that Boston’s next chapter still starts with Tatum.

## Main Question

What did Jayson Tatum’s return show about his post-injury trajectory and Boston’s future around him?

## Data Sources

* NBA.com
* hoopR / NBA Stats API
* Databallr
* ESPN Analytics
* Basketball Reference
* Manually entered playoff leaderboard, rebounding, on/off, playmaking, and Net Points data

## Tools Used

* R
* tidyverse
* ggplot2
* R Markdown
* hoopR
* janitor
* lubridate
* kableExtra
* ggimage
* ggrepel
* gt
* showtext / sysfonts
* zoo

## Key Outputs

* Knitted PDF report
* Cleaned R Markdown analysis file
* Regular-season pre/post-return comparison table
* Post-return scoring trend chart
* Regular-season on/off impact table
* Post-return rebounding leaderboard
* Defensive rebounding percentage chart
* Playmaking indicators table
* Usage vs. true shooting league-context chart
* Playoff production table
* Playoff rankings table
* Celtics playoff Net Points chart
* Playoff on/off impact table

## Notes and Limitations

This project combines live NBA Stats API pulls with manually entered source extracts. The game-log and league-context visuals depend on `hoopR`, so those sections may render fallback plots if the NBA Stats API is unavailable, rate-limited, or blocked. Several tables use manually entered data from Databallr, ESPN Analytics, Basketball Reference, and NBA.com, so the report is not fully reproducible from raw public data alone. The analysis is also built around a small post-return and playoff sample, so shooting efficiency and lineup impact should be interpreted with injury timeline, role context, and opponent context in mind. The post-return scoring chart depends on local logo files stored in `assets/NBA-Team-Logos/`.

## File Guide

* `analysis.Rmd` — cleaned source analysis
* `analysis.pdf` — rendered report
* `README.md` — project overview
* `assets/NBA-Team-Logos/` — local team logo files used in the post-return scoring chart
