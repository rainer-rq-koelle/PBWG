# Read-only consumer of the upstream NM/APDF archive. No cleaning or publishing.
aviation_r2_client <- function() {
  vars <- paste0("AVIATION_R2_", c("ACCESS_KEY_ID", "SECRET_ACCESS_KEY", "ENDPOINT", "BUCKET"))
  if (!all(nzchar(Sys.getenv(vars)))) stop("Set all four AVIATION_R2_* variables in .Renviron.")
  if (!requireNamespace("paws.storage", quietly = TRUE)) stop("Install paws.storage for R2 downloads.")
  paws.storage::s3(config = list(
    credentials = list(creds = list(access_key_id = Sys.getenv(vars[[1]]),
                                    secret_access_key = Sys.getenv(vars[[2]]))),
    endpoint = Sys.getenv(vars[[3]]), region = "auto", max_retries = 2,
    connect_timeout = 15))
}

read_aviation_manifest <- function(path, dataset) {
  m <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(
    .default = readr::col_character(), year = readr::col_integer(),
    rows = readr::col_double(), bytes = readr::col_double(), complete = readr::col_logical()))
  required <- c("dataset", "key", "year", "airport", "quarter", "rows", "bytes", "md5",
                "coverage_from", "coverage_to", "complete", "schema", "written_utc")
  if (!all(required %in% names(m)) || nrow(readr::problems(m))) stop("Invalid archive manifest.")
  if (!nrow(m) || anyNA(m[c("key", "dataset", "year", "rows", "bytes", "md5")]) ||
      any(m$dataset != dataset) || anyDuplicated(m$key) ||
      any(!grepl(paste0("^", dataset, "/year=[0-9]{4}/"), m$key)) ||
      any(grepl("(^|/)\\.\\.(/|$)|\\\\", m$key)) ||
      any(!grepl("[.]parquet$", m$key)) || any(!grepl("^[0-9a-f]{32}$", m$md5)) ||
      any(m$bytes <= 0 | m$rows <= 0)) stop("Invalid archive manifest keys, sizes or checksums.")
  if (dataset == "apdf" && (anyNA(m$airport) || anyDuplicated(m[c("year", "airport")]))) {
    stop("APDF manifest must have one file per airport/year.")
  }
  if (dataset == "nm-flt" && (anyNA(m$quarter) || anyDuplicated(m[c("year", "quarter")]))) {
    stop("NM manifest must have one file per quarter/year.")
  }
  m
}

select_aviation_files <- function(m, dataset, year, airports = NULL) {
  selected <- m[m$year == year, ]
  if (dataset == "apdf") {
    selected <- selected[selected$airport %in% airports, ]
    if (!length(airports) || !setequal(selected$airport, airports)) stop("Missing selected airport/year in manifest.")
  }
  if (!nrow(selected)) stop("No selected files in archive manifest.")
  selected
}

cache_aviation_files <- function(cache, dataset, year, airports = NULL,
                                 client = aviation_r2_client(), bucket = Sys.getenv("AVIATION_R2_BUCKET")) {
  if (!dataset %in% c("apdf", "nm-flt")) stop("Dataset must be apdf or nm-flt.")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  manifest <- file.path(cache, paste0(dataset, "-manifest.csv"))
  # Pin once per cache directory. A later refresh uses a new directory.
  if (!file.exists(manifest)) {
    obj <- client$get_object(Bucket = bucket, Key = paste0(dataset, "/_manifest/manifest.csv"))
    temp <- tempfile("manifest-", tmpdir = cache)
    writeBin(obj$Body, temp)
    read_aviation_manifest(temp, dataset)
    if (!file.rename(temp, manifest)) stop("Cannot save pinned manifest.")
  }
  m <- read_aviation_manifest(manifest, dataset)
  selected <- select_aviation_files(m, dataset, year, airports)
  for (i in seq_len(nrow(selected))) {
    row <- selected[i, ]
    path <- file.path(cache, row$key)
    if (!file.exists(path)) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      temp <- paste0(path, ".partial")
      message("Downloading ", row$key, " (", round(row$bytes / 1e6, 1), " MB)")
      obj <- client$get_object(Bucket = bucket, Key = row$key)
      writeBin(obj$Body, temp)
      rm(obj)
      if (file.info(temp)$size != row$bytes || unname(tools::md5sum(temp)) != row$md5) {
        unlink(temp)
        stop("Downloaded file differs from pinned manifest: ", row$key,
             ". The upstream object may have changed; use a new cache snapshot.")
      }
      if (!file.rename(temp, path)) stop("Cannot finish download: ", row$key)
    }
    if (file.info(path)$size != row$bytes || unname(tools::md5sum(path)) != row$md5) {
      stop("Cached file differs from pinned manifest: ", row$key)
    }
    message("Verified ", row$key)
  }
  invisible(selected)
}

aviation_refresh_sources <- function(cache, dataset, year, airports = NULL) {
  manifest <- normalizePath(file.path(cache, paste0(dataset, "-manifest.csv")), mustWork = TRUE)
  m <- select_aviation_files(read_aviation_manifest(manifest, dataset), dataset, year, airports)
  # written_utc is manifest publication, not extraction or confirmed completeness.
  tibble::tibble(YEAR = m$year, ICAO = if (dataset == "apdf") m$airport else "EUR",
    ARCHIVE = file.path(normalizePath(cache), m$key), MEMBER = NA_character_,
    EXTRACTED_ON = NA_character_, COMPLETE_THROUGH = NA_character_,
    SOURCE_KEY = m$key, SOURCE_MD5 = m$md5, SOURCE_ROWS = m$rows, SOURCE_SCHEMA = m$schema,
    SOURCE_COVERAGE_FROM = m$coverage_from, SOURCE_COVERAGE_TO = m$coverage_to,
    SOURCE_PERIOD_COMPLETE = m$complete, SOURCE_WRITTEN_UTC = m$written_utc,
    MANIFEST_PATH = manifest)
}
