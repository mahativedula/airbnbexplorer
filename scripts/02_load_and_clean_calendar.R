source(file.path("scripts", "helpers.R"))

load_required_packages()
ensure_project_dirs()
check_source_files("calendar")

read_calendar <- function(city) {
  readr::read_csv(
    source_file_path(city, "calendar"),
    show_col_types = FALSE,
    progress = TRUE
  ) |>
    dplyr::mutate(city = city)
}

calendar_clean <- dplyr::bind_rows(
  read_calendar("NYC"),
  read_calendar("LA")
) |>
  trim_character_columns() |>
  dplyr::transmute(
    city = city,
    listing_id = listing_id,
    date = suppressWarnings(lubridate::ymd(date)),
    available = dplyr::case_when(
      available %in% c("t", "true", "TRUE") ~ TRUE,
      available %in% c("f", "false", "FALSE") ~ FALSE,
      TRUE ~ NA
    ),
    price = parse_currency_number(price),
    adjusted_price = parse_currency_number(adjusted_price),
    minimum_nights = suppressWarnings(as.integer(minimum_nights)),
    maximum_nights = suppressWarnings(as.integer(maximum_nights))
  ) |>
  dplyr::filter(!is.na(listing_id), !is.na(date)) |>
  dplyr::mutate(
    year = lubridate::year(date),
    month = lubridate::month(date),
    month_start = as.Date(format(date, "%Y-%m-01")),
    month_label = month_label(month_start),
    week = lubridate::isoweek(date),
    day_of_week = factor(
      lubridate::wday(date, label = TRUE, abbr = FALSE, week_start = 1),
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
      ordered = TRUE
    ),
    is_weekend = day_of_week %in% c("Saturday", "Sunday")
  )

saveRDS(calendar_clean, project_path("data", "calendar_clean.rds"))

message("Saved cleaned calendar to ", project_path("data", "calendar_clean.rds"))
