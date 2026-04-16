source(file.path("scripts", "helpers.R"))

load_required_packages()
ensure_project_dirs()
check_source_files("neighbourhoods")

listings_clean <- readRDS(project_path("data", "listings_clean.rds"))

read_neighbourhoods <- function(city) {
  sf::read_sf(source_file_path(city, "neighbourhoods"), quiet = TRUE) |>
    dplyr::mutate(
      city = city,
      neighbourhood = stringr::str_trim(as.character(neighbourhood)),
      neighbourhood_group = stringr::str_trim(as.character(neighbourhood_group))
    )
}

neighbourhoods_clean <- dplyr::bind_rows(
  read_neighbourhoods("NYC"),
  read_neighbourhoods("LA")
) |>
  dplyr::select(city, neighbourhood_group, neighbourhood, geometry)

neighbourhood_summary <- listings_clean |>
  dplyr::filter(!is.na(neighbourhood)) |>
  dplyr::group_by(city, snapshot_month, neighbourhood_group, neighbourhood) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    median_price = median(price, na.rm = TRUE),
    median_availability_365 = median(availability_365, na.rm = TRUE),
    mean_reviews_per_month = mean(reviews_per_month, na.rm = TRUE),
    share_entire_home = mean(room_type == "Entire home/apt", na.rm = TRUE),
    share_private_room = mean(room_type == "Private room", na.rm = TRUE),
    share_multi_listing_hosts = mean(is_multi_listing_host, na.rm = TRUE),
    .groups = "drop"
  )

host_summary <- listings_clean |>
  dplyr::mutate(
    host_type = dplyr::if_else(is_multi_listing_host, "Multi-listing host", "Single-listing host")
  ) |>
  dplyr::group_by(city, snapshot_month, host_type, room_type) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    median_price = median(price, na.rm = TRUE),
    median_availability_365 = median(availability_365, na.rm = TRUE),
    .groups = "drop"
  )

neighbourhoods_joined <- neighbourhoods_clean |>
  dplyr::left_join(neighbourhood_summary, by = c("city", "neighbourhood_group", "neighbourhood"))

saveRDS(neighbourhoods_clean, project_path("data", "neighbourhoods_clean.rds"))
saveRDS(neighbourhood_summary, project_path("data", "neighbourhood_summary.rds"))
saveRDS(neighbourhoods_joined, project_path("data", "neighbourhoods_joined.rds"))
saveRDS(host_summary, project_path("data", "host_summary.rds"))

message("Saved neighbourhood and host summaries to ", project_path("data"))
