# Azores long-term lakes explorer

Shiny app over the processed Azores lake monitoring tables (coverage, taxon richness, searchable samples). Empty coverage tiles mean that season is **not in this project**, not that a campaign did not happen. 2018 SPR18 macros and chemistry are coded as **Spring** (Mar–Apr).

Live app: *(shinyapps.io URL added after deploy)*

## Run locally

```r
# from this folder
shiny::runApp()
```

Packages: `shiny`, `bslib`, `dplyr`, `ggplot2`, `plotly`, `DT`, `readr`.

## Data

Derived CSVs only (no raw Excel):

- `data/lake_lookup.csv`
- `data/coverage_heatmap_seasonal.csv`
- `data/richness_timeseries_samples.csv`
- `data/richness_timeseries_annual.csv`

Source pipeline: `ms_long_term_azores` (Dropbox analysis).
