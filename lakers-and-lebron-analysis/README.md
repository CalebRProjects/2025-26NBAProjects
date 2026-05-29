# The Lakers Are Peaking at the Right Time

## Overview

This project examines the Los Angeles Lakers’ late-season surge during the 2025-26 season and evaluates whether their improved form reflects a real shift entering the playoffs.

The analysis focuses on how the team stabilized after an uneven start defined by fit questions, defensive concerns, health uncertainty, and changing offensive hierarchy. Luka Dončić and Austin Reaves drove much of the offensive improvement, but the larger story is how LeBron James settled into a connective third-star role while the supporting cast gave the roster more balance.

The project looks at team-level trends, post-All-Star performance, player production, lineup data, and roster context to assess whether the Lakers are simply on a hot streak or genuinely peaking at the right time.

## Main Question

Are the Lakers’ late-season improvements real enough to change how they should be viewed entering the playoffs?

## Data Sources

NBA.com
hoopR
DataBallr

## Tools Used

R
tidyverse
ggplot2
R Markdown
kableExtra
hoopR
zoo
showtext / sysfonts
janitor

## Key Outputs

Knitted PDF report
Cleaned R Markdown analysis file
Season-long margin of victory chart
Rolling 10-game point differential chart
Monthly offensive and defensive rating chart
Best records since January 1 table
Best records since All-Star table
Western Conference standings context table
Post-All-Star offensive rating leaderboard
Post-All-Star defensive rating leaderboard
Post-All-Star net rating leaderboard
Lakers big three post-All-Star production table
Oldest players to average 15+ PPG and 6+ APG table
Support pieces post-All-Star production table
Marcus Smart defensive on/off swing table
Lakers depth-piece production table
High-minute 3-man lineup table

## Notes and Limitations

This report combines live NBA.com data pulled through `hoopR` with manually verified team-rating, standings, and lineup-context tables. Some values are entered directly into the file to preserve verified snapshots from the time of analysis.

The post-All-Star team-rating sections use verified values for offensive rating, defensive rating, and net rating rather than fully recalculating every possession from raw play-by-play data. The lineup section uses DataBallr WOWY mode with padded ratings and low-leverage possessions removed, so those numbers should be interpreted within that specific filtering context.

Because the file was built during an active season, standings, records, player averages, and team rankings should be updated before final publishing. The core analysis is focused on late-season form and playoff readiness, not a full playoff projection model.

## File Guide

`analysis.Rmd` — source analysis file
`analysis.pdf` — rendered report
`oldest-szns-20ppg-6apg.png` — final table from extra context
`README.md` — project overview
