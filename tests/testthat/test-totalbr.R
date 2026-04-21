test_that("Brazil indicator classifier includes official and numeric indicators", {
  expect_equal(
    is_brazil_airport_indicator(c("SBGR", "sdxx", "09AB", "12PL", "AFIL", "SABE", NA)),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("coerce_totalbr_to_apdf_network maps source fields", {
  totalbr <- tibble::tibble(
    dt_dia = as.Date("2025-01-01"),
    co_indicativo = "ABC123",
    co_addep = " sbgr ",
    co_addes = "09ab",
    co_modelo = "A320",
    li_tipovoo = "S",
    TP_VOO_VALIDADO = "G"
  )

  apdf_like <- coerce_totalbr_to_apdf_network(totalbr)

  expect_named(apdf_like, c("FLTID", "ADEP", "ADES", "TYPE", "DATE", "SVC", "TP_VOO_VALIDADO"))
  expect_equal(apdf_like$ADEP, "SBGR")
  expect_equal(apdf_like$ADES, "09AB")
  expect_equal(apdf_like$DATE, as.Date("2025-01-01"))
})

test_that("prepare_totalbr_regional_traffic handles Brazil and AFIL classification", {
  totalbr <- tibble::tibble(
    dt_dia = as.Date(rep("2025-01-01", 5)),
    co_indicativo = paste0("FLT", 1:5),
    co_addep = c("SBGR", "KJFK", "SBGR", "KJFK", "AFIL"),
    co_addes = c("KJFK", "SBGR", "09AB", "EGLL", "SBRJ"),
    co_modelo = "A320",
    li_tipovoo = "S"
  )

  summary <- prepare_totalbr_regional_traffic(totalbr)

  expect_equal(summary$FLTS, 5L)
  expect_equal(summary$D, 1L)
  expect_equal(summary$A, 1L)
  expect_equal(summary$I, 2L)
  expect_equal(summary$O, 1L)
  expect_equal(summary$NN, 5L)
  expect_equal(summary$MS_NA, 5L)
  expect_equal(summary$OTHER, 5L)
})
