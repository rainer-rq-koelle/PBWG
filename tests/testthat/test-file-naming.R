test_that("build_output_filename supports a single year", {
  expect_equal(
    build_output_filename("apdf_egll", 2024),
    "apdf_egll_2024.csv"
  )
})

test_that("build_output_filename supports year ranges and variants", {
  expect_equal(
    build_output_filename("ref_times", c(2019, 2021), suffix = "algo_a"),
    "ref_times_2019-2021_algo_a.csv"
  )
})
