# March Momentum: Mickey Mouse March

## Overview

This project analyzes late-season NBA scoring spikes, or “Mickey Mouse March” runs, across recent seasons to test whether March breakouts usually carry into the following year.

The analysis looks at players who significantly increased their scoring in March compared to their pre-March baseline. It then evaluates whether those jumps were driven by more minutes, higher shot volume, shot-diet changes, free-throw pressure, or short-term shooting spikes.

The goal is to separate real development signals from late-season noise. Some March runs are meaningful, especially when tied to role changes or sustainable shot-profile shifts. Most, however, fade once the next season begins.

## Main Question

Do March scoring jumps actually predict future player growth, or are most late-season heaters temporary opportunity spikes?

## Methodology

This analysis uses NBA player game logs from the 2020-21 through 2025-26 regular seasons.

For the historical carryover analysis, the main sample uses the 2020-21 through 2024-25 seasons. The 2025-26 season is included separately for current-season monitoring and “players to watch” analysis.

Each player-season is split into two windows:

* **Pre-March**: all regular-season games before March
* **March**: all regular-season games played during March

A player qualifies as a March momentum candidate only if they meet the workload and performance filters:

* At least 15 pre-March games
* At least 10 March games
* At least 20 MPG before March
* At least 24 MPG in March
* At least a +3.0 PPG increase in March
* No major efficiency collapse, defined as TS% delta of -2 percentage points or better

For each qualifying player-season, the analysis calculates:

* Pre-March PPG
* March PPG
* PPG delta
* Pre-March TS%
* March TS%
* TS% delta
* MPG delta
* FGA per game delta
* Three-point attempt share delta
* Free-throw rate delta
* Turnover delta
* Usage proxy delta

The usage proxy is calculated as:

`FGA + 0.44 * FTA + TOV`

This is not a full usage-rate estimate, but it gives a simple approximation of offensive possession involvement using game-log data.

## Leap-Type Classification

Each March scoring jump is assigned a descriptive leap type based on the surrounding indicators.

* **Shooting spike**: TS% rises meaningfully while shot volume stays mostly stable
* **Role expansion**: FGA and MPG both increase meaningfully
* **Shot diet shift (3s)**: three-point attempt share increases meaningfully
* **Shot diet shift (FTs)**: free-throw rate increases meaningfully
* **Combo / growth**: no single factor clearly explains the jump

These labels are descriptive, not definitive. They are meant to help interpret why a March spike happened, not prove causality.

## Carryover Rules

To evaluate whether March momentum lasted, each candidate is matched to the next season.

Next-season scoring is compared against both the player’s pre-March baseline and March peak. Each player is placed into one of four outcomes:

* **Held peak**: next-season PPG stayed within 1 point of the March scoring level
* **New baseline**: next-season PPG was at least 2 points above the pre-March baseline
* **Minor bump**: next-season PPG was at least 0.5 points above the pre-March baseline
* **No carry**: next-season PPG returned close to or below the pre-March baseline

This structure treats carryover as a question of baseline change, not just whether a player repeated one hot month exactly.

## Data Sources

* NBA.com
* hoopR
* NBA player game logs
* NBA headshot CDN for player images

## Tools Used

* R
* tidyverse
* ggplot2
* R Markdown
* hoopR
* ggimage
* ggrepel
* knitr
* showtext / sysfonts
* lubridate
* scales
* purrr

## Key Outputs

* Knitted PDF report
* Cleaned R Markdown analysis file
* Executive summary of March momentum findings
* Top 10 March scorers table
* Top 10 biggest March scoring jumps table
* Top 10 biggest March TS% jumps table
* March scoring jump versus efficiency change chart
* Carryover outcome bar chart
* Carryover outcome by leap type chart
* March jump versus next-season baseline carry chart
* Current-season recent scoring swings table
* Full March momentum candidate appendix table

## Notes and Limitations

This project is descriptive rather than predictive. It identifies patterns in March scoring jumps and next-season carryover, but it does not fully model role context, injuries, opponent quality, team incentives, lineup changes, or schedule strength.

The March window can be noisy. Some players benefit from tanking teams, injury-created opportunity, softer late-season rotations, or unusual shooting stretches. The analysis tries to account for this by including minutes, shot volume, efficiency, three-point rate, free-throw rate, and usage proxy changes, but those indicators cannot capture everything.

The leap-type classification is rule-based. It helps organize the findings, but it should not be read as a perfect diagnosis of why every player improved.

The carryover analysis uses next-season scoring average as the main outcome. That makes the results easy to understand, but it does not capture defense, efficiency, playmaking, role changes, or injuries that may affect a player’s value.

The 2025-26 section is separate from the historical carryover sample because the following season is not yet available. Those players should be treated as watch-list candidates, not as confirmed carryover cases.

## File Guide

* `analysis.Rmd` — source analysis file
* `analysis.pdf` — rendered report
* `march-jumps-plot.png` — largest March risers plot
* `README.md` — project overview
