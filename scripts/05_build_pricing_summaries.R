source(file.path("scripts", "helpers.R"))

load_required_packages()
ensure_project_dirs()

listing_snapshot_dir <- function(city) {
  file.path(project_path("data-raw", city), paste(city, "Listings"))
}

list_snapshot_listing_files <- function(city) {
  dir_path <- listing_snapshot_dir(city)

  if (!dir.exists(dir_path)) {
    stop("Missing listing snapshot directory: ", dir_path, call. = FALSE)
  }

  files <- list.files(
    dir_path,
    pattern = "_listings\\.csv$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop("No listing snapshot files found in ", dir_path, call. = FALSE)
  }

  files
}

snapshot_month_from_path <- function(path) {
  file_name <- basename(path)
  match <- stringr::str_match(file_name, "_(\\d{4})_(\\d{1,2})_listings\\.csv$")

  if (is.na(match[1, 1])) {
    stop("Could not parse snapshot month from file name: ", file_name, call. = FALSE)
  }

  as.Date(sprintf("%s-%02d-01", match[1, 2], as.integer(match[1, 3])))
}

read_snapshot_file <- function(city, path) {
  snapshot_month <- snapshot_month_from_path(path)

  readr::read_csv(path, show_col_types = FALSE, progress = FALSE) |>
    trim_character_columns() |>
    dplyr::transmute(
      city = city,
      snapshot_month = snapshot_month,
      listing_id = id,
      host_id = host_id,
      neighbourhood_group = dplyr::na_if(neighbourhood_group_cleansed, ""),
      neighbourhood = dplyr::na_if(neighbourhood_cleansed, ""),
      room_type = dplyr::na_if(room_type, ""),
      property_type = dplyr::na_if(property_type, ""),
      accommodates = suppressWarnings(as.integer(accommodates)),
      bedrooms = suppressWarnings(as.numeric(bedrooms)),
      beds = suppressWarnings(as.numeric(beds)),
      price = parse_currency_number(price),
      minimum_nights = suppressWarnings(as.integer(minimum_nights)),
      availability_30 = suppressWarnings(as.integer(availability_30)),
      availability_60 = suppressWarnings(as.integer(availability_60)),
      availability_90 = suppressWarnings(as.integer(availability_90)),
      availability_365 = suppressWarnings(as.integer(availability_365)),
      number_of_reviews = suppressWarnings(as.integer(number_of_reviews)),
      number_of_reviews_ltm = suppressWarnings(as.integer(number_of_reviews_ltm)),
      estimated_occupancy_l365d = suppressWarnings(as.numeric(estimated_occupancy_l365d)),
      estimated_revenue_l365d = suppressWarnings(as.numeric(estimated_revenue_l365d)),
      review_scores_rating = suppressWarnings(as.numeric(review_scores_rating)),
      review_scores_location = suppressWarnings(as.numeric(review_scores_location)),
      host_is_superhost = host_is_superhost %in% c("t", "true", "TRUE"),
      instant_bookable = instant_bookable %in% c("t", "true", "TRUE"),
      host_listing_count = suppressWarnings(as.integer(calculated_host_listings_count))
    ) |>
    dplyr::mutate(
      is_multi_listing_host = !is.na(host_listing_count) & host_listing_count > 1
    )
}

snapshot_listings <- dplyr::bind_rows(
  lapply(c("NYC", "LA"), function(city) {
    dplyr::bind_rows(lapply(list_snapshot_listing_files(city), function(path) read_snapshot_file(city, path)))
  })
)

usable_months <- snapshot_listings |>
  dplyr::filter(!is.na(price), price > 0) |>
  dplyr::distinct(city, snapshot_month) |>
  dplyr::count(snapshot_month, name = "city_count") |>
  dplyr::filter(city_count == 2) |>
  dplyr::pull(snapshot_month)

shared_snapshot_listings <- snapshot_listings |>
  dplyr::filter(snapshot_month %in% usable_months)

pricing_listings_clean <- shared_snapshot_listings |>
  dplyr::filter(!is.na(price), price > 0)

snapshot_overview_summary <- shared_snapshot_listings |>
  dplyr::filter(!is.na(neighbourhood)) |>
  dplyr::group_by(city, snapshot_month, neighbourhood_group, neighbourhood) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    multi_share = mean(is_multi_listing_host, na.rm = TRUE),
    median_availability = median(availability_365, na.rm = TRUE),
    .groups = "drop"
  )

snapshot_room_type_summary <- shared_snapshot_listings |>
  dplyr::filter(!is.na(room_type)) |>
  dplyr::count(city, snapshot_month, room_type, name = "listing_count") |>
  dplyr::group_by(city, snapshot_month) |>
  dplyr::mutate(share = listing_count / sum(listing_count)) |>
  dplyr::ungroup()

snapshot_host_summary <- shared_snapshot_listings |>
  dplyr::mutate(
    host_type = dplyr::case_when(
      is_multi_listing_host ~ "Multi-listing host",
      TRUE ~ "Single-listing host"
    )
  ) |>
  dplyr::group_by(city, snapshot_month, host_type) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    share = dplyr::n() / nrow(shared_snapshot_listings[shared_snapshot_listings$city == dplyr::first(city) & shared_snapshot_listings$snapshot_month == dplyr::first(snapshot_month), ]),
    median_availability = median(availability_365, na.rm = TRUE),
    .groups = "drop"
  )

pricing_city_summary <- pricing_listings_clean |>
  dplyr::group_by(city, snapshot_month) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    median_price = median(price, na.rm = TRUE),
    mean_price = mean(price, na.rm = TRUE),
    median_availability_365 = median(availability_365, na.rm = TRUE),
    share_multi_listing_hosts = mean(is_multi_listing_host, na.rm = TRUE),
    .groups = "drop"
  )

pricing_room_type_summary <- pricing_listings_clean |>
  dplyr::filter(!is.na(room_type)) |>
  dplyr::group_by(city, snapshot_month, room_type) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    median_price = median(price, na.rm = TRUE),
    mean_price = mean(price, na.rm = TRUE),
    median_availability_365 = median(availability_365, na.rm = TRUE),
    .groups = "drop"
  )

pricing_neighbourhood_summary <- pricing_listings_clean |>
  dplyr::filter(!is.na(neighbourhood)) |>
  dplyr::group_by(city, snapshot_month, neighbourhood_group, neighbourhood) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    median_price = median(price, na.rm = TRUE),
    mean_price = mean(price, na.rm = TRUE),
    median_availability_365 = median(availability_365, na.rm = TRUE),
    share_multi_listing_hosts = mean(is_multi_listing_host, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(listing_count >= 25)

pricing_host_summary <- pricing_listings_clean |>
  dplyr::group_by(city, snapshot_month, is_multi_listing_host, host_is_superhost) |>
  dplyr::summarise(
    listing_count = dplyr::n(),
    median_price = median(price, na.rm = TRUE),
    mean_price = mean(price, na.rm = TRUE),
    median_availability_365 = median(availability_365, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(shared_snapshot_listings, project_path("data", "shared_snapshot_listings.rds"))
saveRDS(snapshot_overview_summary, project_path("data", "snapshot_overview_summary.rds"))
saveRDS(snapshot_room_type_summary, project_path("data", "snapshot_room_type_summary.rds"))
saveRDS(snapshot_host_summary, project_path("data", "snapshot_host_summary.rds"))
saveRDS(pricing_listings_clean, project_path("data", "pricing_listings_clean.rds"))
saveRDS(pricing_city_summary, project_path("data", "pricing_city_summary.rds"))
saveRDS(pricing_room_type_summary, project_path("data", "pricing_room_type_summary.rds"))
saveRDS(pricing_neighbourhood_summary, project_path("data", "pricing_neighbourhood_summary.rds"))
saveRDS(pricing_host_summary, project_path("data", "pricing_host_summary.rds"))
saveRDS(usable_months, project_path("data", "pricing_shared_months.rds"))

message("Saved shared-month summaries for: ", paste(format(usable_months, "%b %Y"), collapse = ", "))
