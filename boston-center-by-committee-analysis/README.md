# Boston’s Center Patchwork: Center-by-Committee Without Redundant Skillsets

## Overview

This project evaluates how Boston has handled its center rotation after losing its previous championship frontcourt infrastructure. The analysis frames Neemias Queta, Luka Garza, and Nikola Vučević as a center-by-committee solution built around complementary skill sets rather than a single replacement. It focuses on role separation, minutes distribution, on/off impact, team identity preservation, and how the rotation could look once Jayson Tatum returns.

## Main Question

Can Boston cover the center position through complementary roles without compromising its spacing, rebounding, defensive structure, or offensive identity?

## Data Sources

- NBA.com
- hoopR / NBA Stats API
- Databallr

## Tools Used

- R
- tidyverse
- ggplot2
- R Markdown
- hoopR
- kableExtra
- ggimage
- ggrepel
- showtext / sysfonts

## Key Outputs

- Knitted PDF report
- Cleaned R Markdown analysis file
- Center production snapshot table
- Pre/post Vučević center minutes share table
- Center minutes distribution chart
- Center on/off net rating chart
- On/off mechanism table
- Standardized role-marker chart with player headshots
- Team identity proxy table and chart
- Rebounding and assist context tables

## Notes and Limitations

This report depends on live NBA Stats API pulls through `hoopR`, with optional local caching controlled by the YAML parameters. If NBA.com changes endpoint schemas, blocks requests, or rate-limits calls, the report may require cached `.rds` files or small code adjustments. Vučević’s Boston sample is intentionally treated as descriptive only because his minutes are very limited. Several context tables are manually entered, so the report is not fully reproducible from raw public data alone.

## File Guide

- `analysis.Rmd` — cleaned source analysis
- `analysis.pdf` — rendered report
- `README.md` — project overview
