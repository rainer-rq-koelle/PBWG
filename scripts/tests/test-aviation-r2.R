source(test_path("..", "lib", "aviation-r2.R"), local = TRUE)

test_that("archive cache pins and verifies content before making it available", {
  folder <- withr::local_tempdir()
  payload <- charToRaw("test payload")
  original <- file.path(folder, "original")
  writeBin(payload, original)
  m <- tibble::tibble(dataset = "apdf", key = "apdf/year=2026/airport=EGLL/EGLL.parquet",
    year = 2026L, airport = "EGLL", quarter = NA_character_, rows = 2, bytes = length(payload),
    md5 = unname(tools::md5sum(original)), coverage_from = "2026-01-01", coverage_to = "2026-09-30",
    complete = FALSE, schema = "fltid-2026-10-30", written_utc = "2026-11-02T10:00:00Z")
  manifest <- file.path(folder, "manifest.csv")
  readr::write_csv(m, manifest)
  manifest_raw <- readBin(manifest, "raw", n = file.info(manifest)$size)
  requests <- character()
  client <- list(get_object = function(Bucket, Key) {
    requests <<- c(requests, Key)
    list(Body = if (endsWith(Key, "manifest.csv")) manifest_raw else payload)
  })
  cache <- file.path(folder, "cache")
  cache_aviation_files(cache, "apdf", 2026, "EGLL", client, "fixture")
  expect_length(requests, 2)
  cache_aviation_files(cache, "apdf", 2026, "EGLL", client, "fixture")
  expect_length(requests, 2) # Reuse pinned, checked files without another request.
  sources <- aviation_refresh_sources(cache, "apdf", 2026, "EGLL")
  expect_true(is.na(sources$EXTRACTED_ON))
  expect_true(is.na(sources$COMPLETE_THROUGH))
  expect_identical(sources$SOURCE_MD5, m$md5)
  writeBin(charToRaw("corrupt"), file.path(cache, m$key))
  expect_error(cache_aviation_files(cache, "apdf", 2026, "EGLL", client, "fixture"), "Cached file differs")
  broken <- list(get_object = function(Bucket, Key) list(Body =
    if (endsWith(Key, "manifest.csv")) manifest_raw else charToRaw("bad")))
  bad_cache <- file.path(folder, "bad-cache")
  expect_error(cache_aviation_files(bad_cache, "apdf", 2026, "EGLL", broken, "fixture"), "Downloaded file differs")
  expect_false(file.exists(file.path(bad_cache, m$key)))
  expect_error(select_aviation_files(m, "apdf", 2026, c("EGLL", "LPPT")), "Missing selected")
  m$key <- "apdf/year=2026/../../outside.parquet"
  readr::write_csv(m, manifest)
  expect_error(read_aviation_manifest(manifest, "apdf"), "Invalid archive manifest")
})
