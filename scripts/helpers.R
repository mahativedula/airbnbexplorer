required_packages <- c("dplyr", "readr", "stringr", "lubridate", "sf")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    y
  } else {
    x
  }
}

load_required_packages <- function() {
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install them before running the data scripts.",
      call. = FALSE
    )
  }

  invisible(lapply(required_packages, library, character.only = TRUE))
}

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

project_path <- function(...) {
  normalizePath(file.path(project_root, ...), winslash = "/", mustWork = FALSE)
}

default_source_dirs <- list(
  NYC = Sys.getenv(
    "AIRBNB_DATA_NYC_DIR",
    unset = project_path("data-raw", "NYC")
  ),
  LA = Sys.getenv(
    "AIRBNB_DATA_LA_DIR",
    unset = project_path("data-raw", "LA")
  )
)

city_file_map <- list(
  NYC = list(
    listings = "NYC listings.csv",
    calendar = "NYC calendar.csv",
    reviews = "NYC reviews.csv",
    neighbourhoods = "NYC neighbourhoods.geojson"
  ),
  LA = list(
    listings = "LA listings.csv",
    calendar = "LA calendar.csv",
    reviews = "LA reviews.csv",
    neighbourhoods = "LA neighbourhoods.geojson"
  )
)

ensure_project_dirs <- function() {
  dir.create(project_path("data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(project_path("data-raw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(project_path("scripts"), recursive = TRUE, showWarnings = FALSE)
}

source_file_path <- function(city, dataset_name) {
  city <- toupper(city)

  if (!city %in% names(default_source_dirs)) {
    stop("Unsupported city: ", city, call. = FALSE)
  }

  if (!dataset_name %in% names(city_file_map[[city]])) {
    stop("Unsupported dataset type: ", dataset_name, call. = FALSE)
  }

  file.path(default_source_dirs[[city]], city_file_map[[city]][[dataset_name]])
}

check_source_files <- function(dataset_name) {
  missing_paths <- vapply(
    names(default_source_dirs),
    function(city) source_file_path(city, dataset_name),
    character(1)
  )

  missing_paths <- missing_paths[!file.exists(missing_paths)]

  if (length(missing_paths) > 0) {
    stop(
      "Missing source files:\n",
      paste(missing_paths, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

parse_currency_number <- function(x) {
  readr::parse_number(as.character(x), locale = readr::locale(grouping_mark = ",", decimal_mark = "."))
}

trim_character_columns <- function(df) {
  character_cols <- names(df)[vapply(df, is.character, logical(1))]
  df[character_cols] <- lapply(df[character_cols], stringr::str_trim)
  df
}

month_label <- function(x) {
  factor(
    format(x, "%b %Y"),
    levels = unique(format(sort(unique(x)), "%b %Y")),
    ordered = TRUE
  )
}
