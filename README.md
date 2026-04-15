# Airbnb Explorer

Interactive Shiny app for comparing Airbnb market patterns in New York City and Los Angeles using Inside Airbnb data.

## Project Structure

- `scripts/` preprocessing scripts that clean the raw files and build summary tables
- `data-raw/` local raw input files for each city and shared-month pricing snapshots
- `data/` generated cleaned data files used by the app
- `ui.R` and `server.R` Shiny app files

## Required Packages

Install these packages in R before running the project:

```r
install.packages(c("dplyr", "readr", "stringr", "lubridate", "sf", "shiny", "tidyverse", "scales"))
```

## Raw Data Setup

The current project uses two kinds of local data inputs:

1. Baseline files used for the original cleaning pipeline
2. Detailed listing snapshots used for the shared-month pricing comparison

Expected structure:

```text
data-raw/
  NYC/
    NYC listings.csv
    NYC calendar.csv
    NYC reviews.csv
    NYC_neighbourhoods.geojson
    NYC Listings/
      NYC_2025_06_listings.csv
      NYC_2025_09_listings.csv
      NYC_2025_12_listings.csv
    NYC Calender/
      NYC_2025_06_calendar.csv
      NYC_2025_09_calendar.csv
      NYC_2025_12_calendar.csv
  LA/
    LA listings.csv
    LA calendar.csv
    LA reviews.csv
    LA_neighbourhoods.geojson
    LA Listings/
      LA_2025_6_listings.csv
      LA_2025_09_listings.csv
      LA_2025_12_listings.csv
    LA Calender/
      LA_2025_06_calendar.csv
      LA_2025_09_calendar.csv
      LA_2025_12_calendar.csv
```

Notes:

- The app's pricing section currently uses the shared priced snapshots for June 2025 and September 2025.
- December snapshot files can still be kept locally, but they are not used in the current pricing visuals because both cities had missing price values there.

Optional: if the baseline raw files are stored somewhere else, set these environment variables before running the scripts:

```r
Sys.setenv(
  AIRBNB_DATA_NYC_DIR = "path/to/NYC",
  AIRBNB_DATA_LA_DIR = "path/to/LA"
)
```

## Build Cleaned Data

Run the original preprocessing pipeline if you need the baseline cleaned files:

```r
source("scripts/run_all.R")
```

Run the pricing snapshot pipeline for the shared-month comparisons:

```r
source("scripts/run_pricing_summaries.R")
```

This generates local `.rds` files in `data/`.

## Run the App

After preprocessing finishes, launch the Shiny app from the project root:

```r
shiny::runApp()
```
