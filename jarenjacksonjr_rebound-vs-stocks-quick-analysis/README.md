# Overview
This project examines Jaren Jackson Jr.’s defensive profile through a specific and unusual lens: the frequency with which his **stocks (steals + blocks)** exceed his rebounds, and what that pattern reveals about his value, his limitations, and how Memphis should build around him.  

The analysis is motivated by recent reporting that the Grizzlies front office is open to moving on from Ja Morant and reshaping the roster around Jaren Jackson Jr. and their young core. Rather than treating this as a hypothetical, the project tests whether a JJJ-centered future is structurally sound based on how he actually impacts games and lineups.  

The work combines game-level data, rebounding distributions, peer comparisons, and early lineup signals to move from individual player traits to team-building implications in a potential post–Morant era.

# Purpose
The goal of this project is:  

1. Quantify a defining pattern.
- Measure how often JJJ records more stocks than rebounds, and track how that changes over time across regular season and playoffs. This clarifies whether his defensive identity is anecdotal or persistent.  

2. Evaluate his rebounding profile in context.
- Place his rebounding alongside similarly sized peers using REB/36 to determine whether his weakness on the glass is stylistic, situational, or structural.  

3. Translate player traits into roster rules.
- Use lineup evidence, particularly JJJ with and without Zach Edey, to clarify what Memphis would need at center and on the perimeter if JJJ is the franchise’s long-term defensive anchor.


# Method & Data  

## Player Game Logs  
- Sourced from the NBA API via `hoopR`.  
- Includes all regular-season games across JJJ’s career and playoffs where relevant to stocks analysis.  
- Key derived metrics:  
  - **Stocks = STL + BLK**  
  - Game-level indicator for **Stocks > Rebounds**  
  - Rebounding “buckets” (≤3, ≥5, ≥7 rebounds) to capture volatility rather than averages.  

## Peer Context (REB/36)  
- Seasonal peer datasets of players **6'8"+** meeting minimum games/minutes thresholds.  
- Standardized REB/36 columns across seasons.  
- JJJ is highlighted against the full distribution each year to show where he consistently falls relative to his size cohort.  

## Lineup Evidence  
- Manually entered WOWY-style indicators comparing:  
  - **JJJ + Zach Edey**  
  - **JJJ on, Edey off**  
- Focus on net rating, offensive/defensive efficiency, and especially **offensive and defensive rebounding rates (ORB% / DORB allowed)** to connect individual traits to team outcomes in a young, evolving roster.

# Why This Matters
## For Player Evaluation  
JJJ is not a traditional big. His value shows up through rim deterrence, shot contests, and event creation rather than rebounding. Treating him like a conventional center obscures both his elite upside and his real constraints.  

## For Team Construction  
The data suggests a clear trade-off that matters if Memphis centers its future around him:  
- JJJ is most valuable when he can roam as a helper and disrupter.  
- That role requires a **true rebound anchor at the 5** to finish possessions he creates.  
- Early evidence with Zach Edey shows how dramatically team outcomes can shift when that coverage exists.  

## For a Post–Morant Grizzlies Build  
If Memphis moves toward a JJJ-led future with younger players like Edey, Jaylen Wells, Cedric Coward, and Cam Spencer, this analysis clarifies what must be true for that plan to work:  
- **Rebounding coverage at center is non-negotiable.**  
- **Perimeter lineups must bring POA defense and shooting gravity.**  
- **Creation must come from a different archetype**

# Authors
Caleb Ramsey - Statistics & Sports Media and Analytics (Virginia Tech)
