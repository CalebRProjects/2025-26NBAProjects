# Anthony Black: Opportunity, Efficiency & Impact

## Overview

This project evaluates Anthony Black’s 2025-26 development with the Orlando Magic, focusing on how expanded opportunity, improved efficiency, transition value, defensive activity, and on/off impact have shaped his role.

The analysis frames Black as a young guard whose value is clearest when he is attacking advantages rather than creating everything from a standstill. His scoring has grown into the 15-point-per-game range, but the stronger signal is how that production is being generated: transition, cuts, spot-ups, closeout attacks, improved rim finishing, and disruptive defense.

The project also identifies the main limitation in his offensive profile. Black has taken on more on-ball responsibility, but pick-and-roll ball-handler possessions remain inefficient relative to league average. His long-term value depends on whether he can keep improving as a creator while continuing to thrive in the advantage-based roles where he already adds value.

## Main Question

Has Anthony Black’s expanded role translated into real impact, and what parts of his game look most sustainable moving forward?

## Methodology

This project uses NBA.com data exported into Excel files, then analyzed and presented in a rendered PDF report.

The original R Markdown source file is not included in this repo. The repo is therefore structured around the final PDF report and the supporting data files rather than a fully reproducible source-code workflow.

The career snapshot section compares Black’s scoring, shooting efficiency, two-point percentage, free-throw percentage, and relative true shooting across his first three seasons.

The role and skill section tracks year-to-year changes in assist-to-turnover ratio, usage rate, transition frequency, and potential assists. These indicators help show how Black’s role has expanded without completely breaking his decision-making profile.

The offensive playtype section compares Black’s points per possession against league average across his highest-volume play types. This separates where his offense works best from where it still struggles. Transition, cuts, and spot-ups show positive value, while pick-and-roll ball-handler possessions remain the biggest limitation.

The shot-zone section tracks field-goal percentage by zone across multiple seasons. The main focus is Black’s growth at the rim, his corner-three efficiency, and the continued weakness of non-rim paint attempts, midrange shots, and above-the-break threes.

The defensive playtype section evaluates Black’s defensive workload and efficiency allowed by possession type. It shows that his toughest on-ball matchups can produce mixed results, but that his off-ball defense, screen navigation, and activity remain valuable.

The disruption section uses deflections per game to place Black among the league’s highest-activity defenders.

The impact section uses DataBallr on/off data with low-leverage possessions removed to evaluate whether Orlando performs better with Black on the floor.

## Data Sources

* NBA.com
* DataBallr 

## Tools Used

* R
* R Markdown
* ggplot2
* tidyverse
* knitr / kableExtra
* NBA.com exported tables
* DataBallr research data

## Key Outputs

* Rendered PDF report
* Career progression table
* Usage and creation changes table
* Offensive playtype PPP versus league average table
* Offensive playtype PPP versus league average chart
* Shot-zone trend table
* Shot-zone FG% trend chart
* Defensive playtype PPP allowed versus league average table
* Defensive playtype PPP allowed chart
* Deflections per game leaderboard
* DataBallr on/off impact table
* Final role and impact summary

## Notes and Limitations

The original R Markdown file for this project is not included in the repo. The final PDF report is preserved, along with the supporting data files. Because of that, this repo should be treated as a report-first portfolio project rather than a fully reproducible code project.

The data files referenced in the project come from NBA.com exports and DataBallr research. Some tables may have been cleaned or reformatted after export to make them easier to use in the report.

The playtype and shot-zone sections are descriptive. They show where Black has been efficient or inefficient, but they do not fully capture lineup context, opponent quality, teammate availability, shot quality, or late-clock burden.

The on/off data helps support the impact case, but it should not be treated as a complete estimate of individual value. Lineup combinations, role, opponent strength, and team context all affect those numbers.

The main purpose of the project is to evaluate Black’s role development and identify where his production looks sustainable. It is not intended to be a full projection model or a definitive ranking of young guards.

## File Guide

* `analysis.pdf` — final rendered report
* `data/` — NBA.com and DataBallr data exports used to build the report
* `README.md` — project overview
