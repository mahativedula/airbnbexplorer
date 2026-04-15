Place repo-local raw data files in this directory before running the preprocessing scripts.

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

The pricing workflow currently uses the detailed listing snapshots from June 2025 and September 2025 because those are the shared months with usable price values in both cities.

Environment variables can be used to override the baseline city locations:

- `AIRBNB_DATA_NYC_DIR`
- `AIRBNB_DATA_LA_DIR`
