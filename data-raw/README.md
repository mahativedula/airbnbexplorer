Place repo-local raw data files in this directory before running the preprocessing scripts.

Expected structure:

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

Environment variables can be used to override these default locations:

- `AIRBNB_DATA_NYC_DIR`
- `AIRBNB_DATA_LA_DIR`
