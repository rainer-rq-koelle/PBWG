test_that("prepare_airport_data filters requested aerodromes", {
  apdf <- tibble::tibble(
    AERODROME = c("EGLL", "LGAV", "EDDF"),
    MOVEMENTS = c(1, 2, 3)
  )

  filtered <- prepare_airport_data(apdf, airports = c("EGLL", "LGAV"))

  expect_equal(filtered$AERODROME, c("EGLL", "LGAV"))
})
