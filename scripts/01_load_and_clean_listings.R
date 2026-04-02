source(file.path("scripts", "helpers.R"))

load_required_packages()
ensure_project_dirs()
check_source_files("listings")

read_listings <- function(city) {
  readr::read_csv(
    source_file_path(city, "listings"),
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    dplyr::mutate(city = city)
}

listings_clean <- dplyr::bind_rows(
  read_listings("NYC"),
  read_listings("LA")
) |>
  trim_character_columns() |>
  dplyr::transmute(
    city = city,
    listing_id = id,
    name = name,
    host_id = host_id,
    host_profile_id = host_profile_id,
    host_name = host_name,
    neighbourhood_group = dplyr::na_if(neighbourhood_group, ""),
    neighbourhood = dplyr::na_if(neighbourhood, ""),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    room_type = dplyr::na_if(room_type, ""),
    price = parse_currency_number(price),
    minimum_nights = suppressWarnings(as.integer(minimum_nights)),
    number_of_reviews = suppressWarnings(as.integer(number_of_reviews)),
    last_review = suppressWarnings(lubridate::ymd(last_review)),
    reviews_per_month = suppressWarnings(as.numeric(reviews_per_month)),
    host_listing_count = suppressWarnings(as.integer(calculated_host_listings_count)),
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
  dplyr::filter(!is.na(listing_id), !is.na(price), price > 0)

saveRDS(listings_clean, project_path("data", "listings_clean.rds"))

message("Saved cleaned listings to ", project_path("data", "listings_clean.rds"))
