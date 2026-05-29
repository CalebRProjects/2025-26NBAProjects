# Scoring Translation in the NBA Playoffs

## Overview

This project analyzes how NBA scoring changes from the regular season to the playoffs from 2000-01 through 2024-25.

The analysis focuses on “scoring translation,” or how well a player’s regular-season scoring carries into the postseason once defenses tighten, matchups become more targeted, and offensive weaknesses are harder to hide. Instead of judging players from one playoff series or one memorable run, the project builds a larger season-by-season and career-level view of who rises, maintains, or drops when the game changes.

The project also separates raw scoring changes from efficiency context. A player increasing playoff scoring is not automatically a better playoff scorer if the jump comes with a major efficiency drop. Likewise, simply maintaining regular-season scoring can be extremely valuable because league scoring generally becomes harder in the playoffs.

## Main Question

Which NBA players have translated their regular-season scoring best to the playoffs since 2000, and what separates playoff risers, maintainers, and droppers?

## Methodology

This analysis compares player scoring in the regular season and playoffs across modern NBA seasons, using only seasons in which a player reached the postseason.

That choice matters. Regular-season production is not taken from a player’s entire career. It is only taken from playoff-qualifying seasons, which keeps the comparison focused on the same version of the player who actually appeared in the postseason.

For each player-season, the analysis calculates:

- Regular-season points per game
- Playoff points per game
- PPG difference
- Percent change in scoring
- Regular-season true shooting percentage
- Playoff true shooting percentage
- Regular-season relative true shooting
- Playoff relative true shooting
- Change in relative true shooting
- Change in shot load
- Change in minutes

Relative true shooting compares a player’s efficiency to league average true shooting in that specific context. Regular-season rTS is measured against that season’s regular-season league average, while playoff rTS is measured against that season’s playoff league average.

This prevents the analysis from treating all eras or scoring environments the same. A 56% true shooting season in 2004 does not mean the same thing as 56% true shooting in 2024.

## Classification Rules

Players are grouped into three broad translation categories.

- **Risers**: players whose scoring increased in the playoffs
- **Maintainers**: players whose scoring stayed within roughly plus or minus 1 point per game
- **Droppers**: players whose scoring declined in the playoffs

Additional filters are used to make the groups more meaningful.

For single-season player comparisons, players must meet minimum regular-season games, playoff games, and regular-season scoring thresholds. This helps remove tiny samples and low-volume players whose scoring changes are less meaningful.

For career-level comparisons, players must meet larger regular-season and playoff game thresholds. This makes the career tables more stable and prevents one short playoff run from defining a player’s translation profile.

For droppers, the analysis attempts to remove obvious role-loss cases by filtering for players who retained meaningful minutes and responsibility. The goal is to capture players whose scoring declined despite still being part of the playoff offense.

## Data Sources

- NBA.com
- hoopR
- Basketball Reference
- Manually entered league true shooting context
- Manually entered all-time playoff leaderboard tables

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- janitor
- kableExtra
- showtext / sysfonts
- forcats
- stringr
- scales

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- League regular-season versus playoff scoring chart
- All-time playoff scoring leaderboard
- All-time playoff rebounding leaderboard
- All-time playoff assists leaderboard
- Modern playoff PPG leaderboard
- Modern playoff RPG leaderboard
- Modern playoff APG leaderboard
- Biggest modern single-run playoff points table
- Biggest modern single-run playoff rebounds table
- Biggest modern single-run playoff assists table
- Biggest modern single-run playoff steals table
- Biggest modern single-run playoff blocks table
- Largest career playoff scoring lifts table
- Best career playoff scoring maintainers table
- Biggest career playoff scoring droppers table
- Biggest single-season playoff risers table
- Best single-season playoff maintainers table
- Biggest single-season playoff droppers table

## Notes and Limitations

This project combines NBA.com data pulled through `hoopR` with manually entered league true shooting values and manually entered all-time leaderboard tables. The modern player-season data is mostly reproducible through the NBA API, but the all-time leaderboards and league true shooting context should be treated as manually curated source inputs.

The analysis uses per-game scoring as the primary translation measure. That makes the results easy to interpret, but it does not fully capture pace, opponent quality, defensive attention, teammate injuries, lineup context, or matchup difficulty.

Relative true shooting helps contextualize efficiency, but it does not fully solve every era or role issue. A player can improve scoring and lose efficiency because his team needed more shot creation. Another player can maintain efficiency because his role stayed narrow. The tables should be read as translation indicators, not final rankings of playoff value.

Single-season playoff samples are naturally volatile. A seven-game shooting slump or one favorable matchup can swing a player’s profile. Career-level tables are more stable, but they can smooth over important changes in role, age, or team context.

The droppers section includes additional filters to avoid obvious role-loss cases, but no filter can perfectly separate poor translation from injury, matchup, or tactical changes.

## File Guide

- `analysis.Rmd` — source analysis file
- `analysis.pdf` — rendered report
- `README.md` — project overview
