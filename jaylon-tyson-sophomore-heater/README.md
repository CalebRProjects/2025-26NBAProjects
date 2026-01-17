# Overview

Jaylon Tyson has emerged as a high-impact bench scorer for Cleveland, offering shooting, versatility, and real volume production at age 23. His recent 39-point performance placed him in extremely rare company among young NBA players.

Since 1996–97, only 10 players have recorded 35+ points, 7+ threes, and 90%+ True Shooting within their first two seasons. Tyson’s game ranks 2nd in efficiency among all such performances.

This repository documents how that group was selected and visualized.

# Purpose

The goal is not to crown Tyson or overstate a single game. Instead, this project:

* Creates a reproducible way to isolate historically rare “heater” performances players.
* Provides transparent criteria for what counts as a Tyson-style game.
* Supplies a tidy dataset and a visual table that can be reused for analysis, graphics, or further research.

# Method & Data

* Data source: NBA Stats via the hoopR R package.
* Coverage is strongest from 1996–97 onward, which defines the analysis window.

Steps:

1. Pull player game logs for every regular season from 1996–97 to the present.
2. Compute True Shooting (TS) for every game.
3. Assign each player a career year (1 = rookie, 2 = sophomore, etc.) based on their first appearance in the data.
4. Keep only games from career years 1–2.
5. Apply the rarity filter:
- PTS ≥ 35
- 3PM ≥ 7
- TS ≥ 90%
6. Deduplicate to one row per player-game.
7. Attach team logos and render a formatted table with Tyson highlighted.

# Why This Matters
On one level, this is simply a fun, historically grounded stat. Very few young players ever combine high volume scoring, three-point shooting, and elite efficiency in the same game.

More importantly, the list reveals the types of players who tend to produce nights like this:
high-volume shooters, secondary creators, and microwaves rather than traditional role players.

The sample is tiny, but that’s the point. These games are outliers by design. But being in this group points to upside (though, there are exceptions), shooting gravity, and offensive ceiling that can matter for player evaluation, development, and projection.

Tyson’s inclusion suggests not just a hot night, but a performance profile that aligns with some very good young scorers in the larger context of NBA history.

# Author
Caleb Ramsey - Statistics & Sports Media and Analytics (Virginia Tech)
