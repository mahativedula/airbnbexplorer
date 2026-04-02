source(file.path("scripts", "helpers.R"))

load_required_packages()
ensure_project_dirs()

calendar_clean <- readRDS(project_path("data", "calendar_clean.rds"))
listings_clean <- readRDS(project_path("data", "listings_clean.rds"))

listing_lookup <- listings_clean |>
  dplyr::select(
    city,
    listing_id,
    room_type,
    neighbourhood_group,
    neighbourhood,
    is_multi_listing_host
  )

calendar_enriched <- calendar_clean |>
  dplyr::left_join(listing_lookup, by = c("city", "listing_id"))

monthly_calendar_summary <- calendar_enriched |>
  dplyr::group_by(city, month_start, month_label, room_type, neighbourhood_group, neighbourhood) |>
  dplyr::summarise(
    active_listing_count = dplyr::n_distinct(listing_id),
    mean_availability_rate = mean(available, na.rm = TRUE),
    median_calendar_price = median(price, na.rm = TRUE),
    mean_calendar_price = mean(price, na.rm = TRUE),
    share_multi_listing_host_inventory = mean(is_multi_listing_host, na.rm = TRUE),
    .groups = "drop"
  )

city_month_summary <- calendar_enriched |>
  dplyr::group_by(city, month_start, month_label) |>
  dplyr::summarise(
    active_listing_count = dplyr::n_distinct(listing_id),
    mean_availability_rate = mean(available, na.rm = TRUE),
    median_calendar_price = median(price, na.rm = TRUE),
    mean_calendar_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(monthly_calendar_summary, project_path("data", "monthly_calendar_summary.rds"))
saveRDS(city_month_summary, project_path("data", "city_month_summary.rds"))

message("Saved calendar summaries to ", project_path("data"))
