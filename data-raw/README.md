Place repo-local raw data files in this directory before running the preprocessing scripts.

If `data/` already contains the generated `.rds` files, the app can run without these raw files.

Current rebuild requirements:

- `scripts/run_all.R`
  - `NYC_neighbourhoods.geojson`
  - `NYC Listings/`
  - `LA_neighbourhoods.geojson`
  - `LA Listings/`
- `scripts/run_pricing_summaries.R`
  - detailed listing snapshots in `NYC Listings/` and `LA Listings/`

The old top-level raw CSV files are not used by the current preprocessing scripts.

Expected structure:

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

The main app now uses listing-based summaries only. Pricing still uses the detailed listing snapshots from June 2025 and September 2025 because those are the shared months with usable price values in both cities. This odd coverage comes from the Inside Airbnb source files, which are not consistently available across the same months for both cities and do not provide usable December price coverage for both markets.

Environment variables can be used to override the city root folders:

- `AIRBNB_DATA_NYC_DIR`
- `AIRBNB_DATA_LA_DIR`

