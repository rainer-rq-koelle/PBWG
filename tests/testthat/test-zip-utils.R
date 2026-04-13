test_that("check_zip_content lists files in a zip archive", {
  tmp_dir <- withr::local_tempdir()
  file_path <- file.path(tmp_dir, "sample.csv")

  writeLines("A,B\n1,2", file_path)
  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "sample.zip", files = "sample.csv")

  listed <- check_zip_content(tmp_dir, "sample.zip")

  expect_true("sample.csv" %in% listed$Name)
})

test_that("read_zip_content reads zipped csv files", {
  tmp_dir <- withr::local_tempdir()
  file_path <- file.path(tmp_dir, "sample.csv")

  writeLines("A,B\n1,2", file_path)
  withr::local_dir(tmp_dir)
  utils::zip(zipfile = "sample.zip", files = "sample.csv")

  data <- read_zip_content(tmp_dir, "sample.zip", type = "csv")

  expect_s3_class(data, "tbl_df")
  expect_named(data, c("A", "B"))
})

test_that("read_csv12 switches to csv2 when needed", {
  tmp_dir <- withr::local_tempdir()
  file_path <- file.path(tmp_dir, "sample.csv")

  writeLines("A;B\n1;2", file_path)

  data <- read_csv12(file_path)

  expect_named(data, c("A", "B"))
  expect_equal(data$A, 1)
})
