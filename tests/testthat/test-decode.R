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
