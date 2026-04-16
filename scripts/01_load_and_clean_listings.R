source(file.path("scripts", "helpers.R"))

load_required_packages()
ensure_project_dirs()
check_snapshot_files("listings")

read_listings_snapshot <- function(city, path) {
  snapshot_month <- snapshot_month_from_path(path, "listings")
  raw <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE) |>
    trim_character_columns()

  dplyr::tibble(
    city = city,
    snapshot_month = snapshot_month,
    listing_id = as.character(column_or_na(raw, c("id"))),
    name = as.character(column_or_na(raw, c("name"))),
    host_id = as.character(column_or_na(raw, c("host_id"))),
    host_profile_id = as.character(column_or_na(raw, c("host_profile_id"))),
    host_name = as.character(column_or_na(raw, c("host_name"))),
    neighbourhood_group = as.character(column_or_na(raw, c("neighbourhood_group_cleansed", "neighbourhood_group"))),
    neighbourhood = as.character(column_or_na(raw, c("neighbourhood_cleansed", "neighbourhood"))),
    latitude = as.character(column_or_na(raw, c("latitude"))),
    longitude = as.character(column_or_na(raw, c("longitude"))),
    room_type = as.character(column_or_na(raw, c("room_type"))),
    price = as.character(column_or_na(raw, c("price"))),
    minimum_nights = as.character(column_or_na(raw, c("minimum_nights"))),
    number_of_reviews = as.character(column_or_na(raw, c("number_of_reviews"))),
    last_review = as.character(column_or_na(raw, c("last_review"))),
    reviews_per_month = as.character(column_or_na(raw, c("reviews_per_month"))),
    host_listing_count = as.character(column_or_na(raw, c("calculated_host_listings_count", "host_listings_count"))),
    availability_365 = as.character(column_or_na(raw, c("availability_365"))),
    number_of_reviews_ltm = as.character(column_or_na(raw, c("number_of_reviews_ltm"))),
    license = as.character(column_or_na(raw, c("license")))
  )
}

listings_clean <- dplyr::bind_rows(
  lapply(c("NYC", "LA"), function(city) {
    dplyr::bind_rows(lapply(list_snapshot_files(city, "listings"), function(path) read_listings_snapshot(city, path)))
  })
) |>
  dplyr::transmute(
    city = city,
    snapshot_month = snapshot_month,
    listing_id = suppressWarnings(as.numeric(listing_id)),
    name = dplyr::na_if(name, ""),
    host_id = suppressWarnings(as.numeric(host_id)),
    host_profile_id = suppressWarnings(as.numeric(host_profile_id)),
    host_name = dplyr::na_if(host_name, ""),
    neighbourhood_group = dplyr::na_if(neighbourhood_group, ""),
    neighbourhood = dplyr::na_if(neighbourhood, ""),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    room_type = dplyr::na_if(room_type, ""),
    price = parse_currency_number(price),
    minimum_nights = suppressWarnings(as.integer(minimum_nights)),
    number_of_reviews = suppressWarnings(as.integer(number_of_reviews)),
    last_review = suppressWarnings(lubridate::ymd(last_review)),
    reviews_per_month = suppressWarnings(as.numeric(reviews_per_month)),
    host_listing_count = suppressWarnings(as.integer(host_listing_count)),
    availability_365 = suppressWarnings(as.integer(availability_365)),
    number_of_reviews_ltm = suppressWarnings(as.integer(number_of_reviews_ltm)),
    license = dplyr::na_if(license, "")
  ) |>
  dplyr::mutate(
    coord_valid = dplyr::between(latitude, -90, 90) & dplyr::between(longitude, -180, 180),
    is_multi_listing_host = !is.na(host_listing_count) & host_listing_count > 1,
    price_log = dplyr::if_else(!is.na(price) & price > 0, log10(price), NA_real_),
    price_band = dplyr::case_when(
      is.na(price) ~ NA_character_,
      price < 100 ~ "<100",
      price < 200 ~ "100-199",
      price < 300 ~ "200-299",
      TRUE ~ "300+"
    ),
    availability_band = dplyr::case_when(
      is.na(availability_365) ~ NA_character_,
      availability_365 <= 30 ~ "0-30",
      availability_365 <= 90 ~ "31-90",
      availability_365 <= 180 ~ "91-180",
      TRUE ~ "181-365"
    ),
    review_activity_band = dplyr::case_when(
      is.na(number_of_reviews_ltm) ~ NA_character_,
      number_of_reviews_ltm == 0 ~ "0",
      number_of_reviews_ltm <= 5 ~ "1-5",
      number_of_reviews_ltm <= 20 ~ "6-20",
      TRUE ~ "21+"
    )
  ) |>
  dplyr::filter(!is.na(listing_id)) |>
  dplyr::distinct(city, snapshot_month, listing_id, .keep_all = TRUE)

saveRDS(listings_clean, project_path("data", "listings_clean.rds"))

message("Saved cleaned listings to ", project_path("data", "listings_clean.rds"))

