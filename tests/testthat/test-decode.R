test_that("decode_nm_flights renames columns based on dictionary", {
  nm_flights <- tibble::tibble(
    DEP_APT = "EGLL",
    ARR_APT = "LGAV",
    PHASE = "AIRBORNE"
  )

  dictionary <- tibble::tibble(
    SOURCE_NAME = c("DEP_APT", "ARR_APT", "PHASE"),
    TARGET_NAME = c("ADEP", "ADES", "PHASE")
  )

  decoded <- decode_nm_flights(nm_flights, dictionary = dictionary)

  expect_named(decoded, c("ADEP", "ADES", "PHASE"))
})

test_that("decode_apdf keeps unmapped columns", {
  apdf <- tibble::tibble(
    ICAO = "EGLL",
    LOCAL_FIELD = 1
  )

  decoded <- decode_apdf(apdf)

  expect_named(decoded, c("ICAO", "LOCAL_FIELD"))
})

test_that("upstream flight identifiers and provenance survive APDF preparation", {
  raw <- tibble::tibble(AP_C_FLTID = c("LX316", "LX999"), FLTID = c("SWR5MP", NA_character_),
    FLTID_AP = c("LX316", "LX999"), FLTID_SOURCE = c("NM", "AP_NO_NM"), NM_MATCH_OK = c(TRUE, NA),
    ADEP_ICAO = "LSZH", ADES_ICAO = "EGLL", SRC_PHASE = "DEP", AC_CLASS = "M",
    MVT_TIME_UTC = as.POSIXct("2026-01-01 12:00:00", tz = "UTC"),
    BLOCK_TIME_UTC = as.POSIXct("2026-01-01 11:45:00", tz = "UTC"))
  for (prepare in list(decode_apdf, prepare_apdf_traffic_input, prepare_apdf_tma_input)) {
    data <- prepare(raw)
    expect_identical(data$FLTID, raw$FLTID)
    expect_identical(data$AP_C_FLTID, raw$AP_C_FLTID)
    expect_identical(data$FLTID_AP, raw$FLTID_AP)
    expect_identical(data$FLTID_SOURCE, raw$FLTID_SOURCE)
    expect_identical(data$NM_MATCH_OK, raw$NM_MATCH_OK)
    expect_false(anyDuplicated(names(data)) > 0)
  }
  legacy <- raw[, !names(raw) %in% c("FLTID", "FLTID_AP", "FLTID_SOURCE", "NM_MATCH_OK")]
  expect_identical(prepare_apdf_traffic_input(legacy)$FLTID, raw$AP_C_FLTID)
})

test_that("decode functions reject duplicate source names", {
  dictionary <- tibble::tibble(
    SOURCE_NAME = c("A", "A"),
    TARGET_NAME = c("B", "C")
  )

  expect_error(
    decode_nm_flights(tibble::tibble(A = 1), dictionary = dictionary),
    "must be unique"
  )
})
