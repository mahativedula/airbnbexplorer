source(file.path("scripts", "helpers.R"))

load_required_packages()
ensure_project_dirs()

listings_clean <- readRDS(project_path("data", "listings_clean.rds"))

snapshot_city_summary <- listings_clean |>
  dplyr::group_by(city, snapshot_month) |>
  dplyr::summarise(
    active_listing_count = dplyr::n_distinct(listing_id),
    mean_availability_365 = mean(availability_365, na.rm = TRUE),
    median_availability_365 = median(availability_365, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    share_multi_listing_hosts = mean(is_multi_listing_host, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    month_start = snapshot_month,
    month_label = month_label(snapshot_month),
    .before = 3
  )

snapshot_room_summary <- listings_clean |>
  dplyr::filter(!is.na(room_type)) |>
  dplyr::group_by(city, snapshot_month, room_type) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    mean_availability_365 = mean(availability_365, na.rm = TRUE),
    median_availability_365 = median(availability_365, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(snapshot_city_summary, project_path("data", "snapshot_city_summary.rds"))
saveRDS(snapshot_room_summary, project_path("data", "snapshot_room_summary.rds"))

message("Saved snapshot summaries to ", project_path("data"))
