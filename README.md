# Airbnb Explorer

Interactive Shiny app for comparing Airbnb market patterns in New York City and Los Angeles using Inside Airbnb data.

## Project Structure

- `scripts/` preprocessing scripts that clean the raw files and build summary tables
- `data-raw/` local raw input files for each city
- `data/` generated cleaned data files used by the app
- `ui.R` and `server.R` Shiny app files

## Required Packages

Install these packages in R before running the project:

```r
install.packages(c("dplyr", "readr", "stringr", "lubridate", "sf", "shiny"))
```

## Raw Data Setup

Download the Inside Airbnb files for New York City and Los Angeles, then place them in this structure:

```text
data-raw/
  NYC/
    NYC listings.csv
    NYC calendar.csv
    NYC reviews.csv
    NYC neighbourhoods.geojson
  LA/
    LA listings.csv
    LA calendar.csv
    LA reviews.csv
    LA neighbourhoods.geojson
```

The preprocessing scripts use those repo-local folders by default.

Optional: if the raw files are stored somewhere else, set these environment variables before running the scripts:

```r
Sys.setenv(
  AIRBNB_DATA_NYC_DIR = "path/to/NYC",
  AIRBNB_DATA_LA_DIR = "path/to/LA"
)
```

## Build Cleaned Data

Run the preprocessing pipeline from the project root:

```r
source("scripts/run_all.R")
```

This generates local `.rds` files in `data/`.

## Run the App

After preprocessing finishes, launch the Shiny app from the project root:

```r
shiny::runApp()
```
