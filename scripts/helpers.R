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
    neighbourhoods = "NYC_neighbourhoods.geojson"
  ),
  LA = list(
    neighbourhoods = "LA_neighbourhoods.geojson"
  )
)

snapshot_subdir_map <- list(
  listings = "Listings"
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

snapshot_dir_path <- function(city, dataset_name) {
  city <- toupper(city)

  if (!city %in% names(default_source_dirs)) {
    stop("Unsupported city: ", city, call. = FALSE)
  }

  if (!dataset_name %in% names(snapshot_subdir_map)) {
    stop("Unsupported snapshot dataset type: ", dataset_name, call. = FALSE)
  }

  file.path(default_source_dirs[[city]], paste(city, snapshot_subdir_map[[dataset_name]]))
}

list_snapshot_files <- function(city, dataset_name) {
  dir_path <- snapshot_dir_path(city, dataset_name)

  if (!dir.exists(dir_path)) {
    stop("Missing snapshot directory: ", dir_path, call. = FALSE)
  }

  pattern <- paste0("_", dataset_name, "\\.csv$")
  files <- list.files(dir_path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No ", dataset_name, " snapshot files found in ", dir_path, call. = FALSE)
  }

  files
}

check_snapshot_files <- function(dataset_name) {
  invisible(lapply(names(default_source_dirs), list_snapshot_files, dataset_name = dataset_name))
}

snapshot_month_from_path <- function(path, dataset_name) {
  file_name <- basename(path)
  pattern <- paste0("_(\\d{4})_(\\d{1,2})_", dataset_name, "\\.csv$")
  match <- stringr::str_match(file_name, pattern)

  if (is.na(match[1, 1])) {
    stop("Could not parse snapshot month from file name: ", file_name, call. = FALSE)
  }

  as.Date(sprintf("%s-%02d-01", match[1, 2], as.integer(match[1, 3])))
}

column_or_na <- function(df, candidates, default = NA) {
  for (candidate in candidates) {
    if (candidate %in% names(df)) {
      return(df[[candidate]])
    }
  }

  rep(default, nrow(df))
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
