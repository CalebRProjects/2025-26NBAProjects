# What is Going On With Paolo Banchero?

## Overview

This project evaluates Paolo Banchero’s offensive role, shot selection, and overall impact during the 2025-26 season.

The analysis focuses on whether Banchero’s current offensive usage is aligned with his strengths. He remains one of the league’s most physically gifted young forwards, but his shot profile has created a major tension: too many possessions end in low-efficiency self-created jump shots, while his best traits are tied to downhill pressure, foul drawing, play finishing, and attacking tilted defenses.

The project looks at Banchero’s shot diet, jump-shooting efficiency, play-type profile, leaguewide usage-efficiency context, Cleaning the Glass on/off indicators, Franz Wagner lineup splits, and defensive impact. The goal is not to dismiss Banchero’s talent, but to identify why his current offensive role may be capping both his individual value and Orlando’s offensive ceiling.

## Main Question

Is Paolo Banchero’s current offensive role helping Orlando maximize his strengths, or is his shot selection limiting his efficiency and overall impact?

## Methodology

This analysis combines NBA.com shot chart data, Synergy-style play-type tables, Cleaning the Glass offensive and on/off context, DataBallr WOWY lineup splits, and CraftedNBA defensive indicators.

The shot-profile section uses NBA.com ShotChartDetail data pulled through `hoopR`. Shots are cached locally and then grouped into standardized court zones, including corner threes, wing threes, top-of-key threes, midrange zones, non-rim paint, and rim attempts. Zones with too few attempts are filtered out so the chart focuses on meaningful shot areas.

The jump-shooting section uses manually entered shot-type and jumper-distance tables. These tables separate Banchero’s overall shot diet into broad shot categories, including jump shots, runners, and rim attempts. The jumper-specific table breaks jump shots into short twos, long twos, and threes to show where the efficiency problem is concentrated.

The play-type section uses manually entered Synergy-style data to evaluate how Banchero performs across offensive contexts. Play types include pick-and-roll ball handler, transition, isolation, spot-up, post-up, cuts, roll-man possessions, and off-screen possessions. This helps separate possessions where Banchero is creating from a standstill from possessions where he is finishing advantages.

The usage-versus-efficiency chart pulls leaguewide NBA.com player dashboard data through `hoopR`. It compares Banchero to other players by field-goal attempts per 36 minutes and true shooting percentage, with a minutes filter applied to remove tiny samples.

The lineup-context section uses Cleaning the Glass and DataBallr WOWY-style data. Cleaning the Glass tables summarize Banchero’s offensive role, usage, assist rate, turnover rate, and on/off four-factor profile across multiple seasons. The WOWY table compares Orlando lineup segments with Banchero and Franz Wagner on or off the floor, with low-leverage possessions removed.

The defensive section uses CraftedNBA indicators to evaluate whether Banchero’s defense offsets his offensive inefficiency. Metrics include rim defense, rim frequency defended, defensive rebounds, block rate, deflections, defensive turnovers forced, versatility, foul rate, and Crafted DPM.

## Data Sources

* NBA.com
* hoopR
* Synergy/SportRadar
* Cleaning the Glass
* DataBallr
* CraftedNBA

## Tools Used

* R
* tidyverse
* ggplot2
* R Markdown
* hoopR
* kableExtra / knitr
* ggrepel
* readr
* lubridate
* showtext / sysfonts
* scales
* stringr

## Key Outputs

* Knitted PDF report
* Cleaned R Markdown analysis file
* Shot distribution and efficiency by shot type table
* Shot chart by standardized court zone
* Jump-shooting efficiency by distance table
* Play-type efficiency table
* Usage versus true shooting league-context chart
* Cleaning the Glass offensive overview table
* Cleaning the Glass on/off four-factor table
* DataBallr Banchero/Wagner WOWY table
* CraftedNBA defensive impact table
* Closing role assessment

## Notes and Limitations

This report combines reproducible NBA.com pulls with manually entered source tables from Synergy-style data, Cleaning the Glass, DataBallr, and CraftedNBA. The NBA.com shot chart and league dashboard sections can be rerun through `hoopR`, but the manually entered tables should be refreshed before final publishing if the source numbers change.

The shot-zone chart uses location-based bins from NBA.com shot coordinates. It is useful for identifying shot diet and zone efficiency, but it does not fully account for shot quality, defender distance, clock context, play design, or teammate spacing.

The play-type tables are descriptive. They show where Banchero has been efficient or inefficient by possession type, but they do not prove causality. A poor isolation or pick-and-roll profile can reflect shot selection, spacing, matchup difficulty, or late-clock burden.

The WOWY and on/off tables should be interpreted carefully. Lineup data is affected by teammate combinations, opponent strength, shooting variance, and substitution patterns. The Banchero/Wagner splits are useful for team-context evaluation, but they are not a complete estimate of individual value.

The defensive section is included to test whether Banchero’s non-scoring value offsets the offensive inefficiency. Those metrics suggest useful defensive traits, especially near the rim, but not enough overall defensive impact to make offensive role concerns irrelevant.

The purpose of the project is not to argue that Banchero is bad. It is to show that Orlando’s offensive ceiling likely depends on shifting him toward a more optimized role built around downhill pressure, advantage finishing, playmaking, and jump shots as counters rather than foundations.

## File Guide

* `analysis.Rmd` — source analysis file
* `analysis.pdf` — rendered report
* `README.md` — project overview
