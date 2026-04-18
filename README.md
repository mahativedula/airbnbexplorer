# Airbnb Explorer

Interactive Shiny app for comparing Airbnb market patterns in New York City and Los Angeles using Inside Airbnb data.

## Project Structure

- `scripts/` preprocessing scripts that clean the raw files and build summary tables
- `data-raw/` local raw input files for each city
- `data/` app-ready processed files used by the dashboard
- `ui.R` and `server.R` Shiny app files

## Required Packages

Install these packages in R before running the project:

```r
install.packages(c("dplyr", "readr", "stringr", "lubridate", "sf", "shiny", "tidyverse", "scales"))
```

## Run the App

The repository is set up so the app can run directly after cloning, as long as the tracked files in `data/` are present.

From the project root, run:

```r
shiny::runApp()
```

## Rebuild the Processed Data

You only need these steps if the tracked `data/` files are missing or if you want to regenerate them from raw source files.

### Raw Data Setup

If you want to rebuild the derived data, the current scripts use:

- neighbourhood boundary files for each city
- three listing snapshots for each city

The old top-level raw CSV files are not used by the current preprocessing pipeline.

Expected local structure:

```text
data-raw/
  NYC/
    NYC_neighbourhoods.geojson
    NYC Listings/
      NYC_2025_06_listings.csv
      NYC_2025_09_listings.csv
      NYC_2025_12_listings.csv
  LA/
    LA_neighbourhoods.geojson
    LA Listings/
      LA_2025_6_listings.csv
      LA_2025_09_listings.csv
      LA_2025_12_listings.csv
```

Notes:

- `scripts/run_all.R` builds the main dashboard summaries from the June, September, and December listing snapshots.
- Listing availability in the app is based on the `availability_365` field in the listing snapshots, not on calendar files.
- The pricing section uses only the shared priced listing snapshots from June 2025 and September 2025.
- December listing snapshots can still stay in place for the broader market summaries even though they are not used in the current pricing charts. This limitation comes from the Inside Airbnb source files: the city coverage is incomplete across months, and December does not provide usable price coverage for both cities.

Optional: if the city folders are stored somewhere else, set these environment variables before running the scripts:

```r
Sys.setenv(
  AIRBNB_DATA_NYC_DIR = "path/to/NYC",
  AIRBNB_DATA_LA_DIR = "path/to/LA"
)
```

### Build Commands

Run the main preprocessing pipeline for the three listing snapshot months:

```r
source("scripts/run_all.R")
```

This writes the listing-based summary files used by the app into `data/`.

Run the pricing snapshot pipeline for the shared-month price comparisons:

```r
source("scripts/run_pricing_summaries.R")
```

This writes the pricing summary files used by the app into `data/`.
