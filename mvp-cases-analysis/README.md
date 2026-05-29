# 2025-26 NBA MVP Case Analysis

## Overview

This project evaluates the top four candidates in the 2025-26 NBA MVP race through individual case-study reports.

The analysis was built around Shai Gilgeous-Alexander, Nikola Jokić, Luka Dončić, and Victor Wembanyama, with each candidate receiving a separate report focused on their statistical profile, team context, offensive and defensive impact, and overall MVP argument.

Rather than treating the MVP race as a simple box-score comparison, this project uses multiple data sources to examine how each player drives value. The goal is to understand not just who has the best numbers, but how those numbers are produced, how they scale within team context, and what makes each candidate’s case distinct.

## Main Question

How do the top MVP candidates compare when their statistical production, impact metrics, team context, play style, and workload are evaluated together?

## Methodology

This project uses a case-study approach. Each MVP candidate is evaluated separately before being considered within the broader award race.

The reports combine traditional statistics, advanced metrics, play-type data, lineup impact, and team-performance context. Each case is built around the specific strengths and questions surrounding that candidate rather than forcing every player into the exact same template.

The analysis considers several broad categories:

* Box-score production
* Scoring efficiency
* Offensive creation burden
* Playmaking value
* Defensive impact
* Team performance
* On/off and lineup context
* Play-type profile
* Shot quality and shot diet
* Sustainability of impact
* Narrative and award context

Because each candidate has a different profile, the reports emphasize different areas. For example, Jokić’s case leans heavily into offensive orchestration and efficiency, SGA’s into scoring pressure and two-way consistency, Luka’s into offensive burden and creation volume, and Wembanyama’s into historic two-way impact and defensive dominance.

## Candidate Reports

* `SGA-mvp-case-analysis/` — Shai Gilgeous-Alexander MVP case
* `Jokic-mvp-case-analysis/` — Nikola Jokić MVP case
* `Luka-mvp-case-analysis/` — Luka Dončić MVP case
* `Wemby-mvp-case-analysis/` — Victor Wembanyama MVP case

## Data Sources

* NBA.com
* DataBallr
* Synergy
* PBPStats.com

## Tools Used

* R
* R Markdown
* tidyverse
* ggplot2
* kableExtra / knitr
* hoopR
* DataBallr research exports
* Synergy play-type data
* PBPStats lineup and possession context
* NBA.com player and team data

## Key Outputs

* Individual MVP case reports for four candidates
* Candidate-specific statistical summaries
* Team-context analysis
* Play-type and shot-profile evaluation
* On/off and lineup-impact context
* Offensive workload and creation analysis
* Defensive-impact sections where relevant
* Comparative MVP framing across the top of the race

## Notes and Limitations

This project is designed as a research-driven MVP case study, not a formal award prediction model. The reports evaluate each candidate’s case using available statistical evidence, film-informed interpretation, and team context.

The data sources vary by section. Some tables and visuals are built from NBA.com or PBPStats data, while others rely on DataBallr or Synergy research exports. Because of that, not every part of the project is fully reproducible from one public data pull alone.

Synergy and DataBallr data may require manual export, access permissions, or source-specific interpretation. Any manually entered or exported values should be refreshed before final publishing if the underlying data changes.

The MVP race is also context-dependent. Team record, injuries, late-season performance, voter narratives, and playoff positioning can all change how a candidate’s case is perceived. These reports should be read as structured analytical arguments rather than definitive rankings.

## File Guide

* `Jokic-mvp-case-analysis/` — Nikola Jokić report files
* `Luka-mvp-case-analysis/` — Luka Dončić report files
* `SGA-mvp-case-analysis/` — Shai Gilgeous-Alexander report files
* `Wemby-mvp-case-analysis/` — Victor Wembanyama report files
* `README.md` — project overview
