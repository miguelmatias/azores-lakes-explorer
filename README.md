# Azores long-term lakes explorer

Shiny app over the processed Azores lake monitoring tables (coverage, taxon richness, searchable samples). Empty coverage tiles mean that season is **not in this project**, not that a campaign did not happen. 2018 SPR18 macros and chemistry are coded as **Spring** (Mar–Apr).

Live app: https://miguelmatias.shinyapps.io/azores-lakes-explorer/

## Run locally

```r
# from this folder
shiny::runApp()
```

Packages: `shiny`, `bslib`, `dplyr`, `ggplot2`, `plotly`, `DT`, `readr`.

## Publish

GitHub (code) plus shinyapps.io (the shareable link):

```sh
# 1. GitHub CLI (once)
gh auth login --hostname github.com --git-protocol ssh --web

# 2. shinyapps.io token from https://www.shinyapps.io/admin/#/tokens (once, in R)
#    rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")

# 3. Create the public repo, push, and deploy
sh publish.sh
```

## Data

Derived CSVs only (no raw Excel):

- `data/lake_lookup.csv`
- `data/coverage_heatmap_seasonal.csv`
- `data/richness_timeseries_samples.csv`
- `data/richness_timeseries_annual.csv`

Source pipeline: `ms_long_term_azores` (Dropbox analysis).
