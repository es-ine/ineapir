test_that("get_metadata_series", {
  skip_on_cran()
  expect_type(get_metadata_series(codSeries = "IPC206449"), "list")
})

test_that("test get_metadata_series_operation", {
  skip_on_cran()
  expect_s3_class(get_metadata_series_operation(operation = "IPC", validate = FALSE), "data.frame")
})

test_that("test get_metadata_series_values", {
  skip_on_cran()
  expect_s3_class(get_metadata_series_values(codSeries = "IPC206449"), "data.frame")
  expect_s3_class(get_metadata_series_values(codSeries = "IPC206449", det = 2), "data.frame")
})

test_that("test get_metadata_series_table", {
  skip_on_cran()
  expect_s3_class(get_metadata_series_table(idTable = 50902, filter = list("3" = "74", "762" = "304092"),
                                                     metanames = TRUE, metacodes = TRUE, tip = "M"), "data.frame")
})

test_that("test get_metadata_series_filter", {
  skip_on_cran()
  expect_s3_class(get_metadata_series_filter(operation = "IPC", periodicity = 1, validate = FALSE,
                                                      filter = list("115"= "28", "3" = "84", "762" = "304092")), "data.frame")
})

test_that("test get_metadata_series_varval", {
  skip_on_cran()
  expect_s3_class(get_metadata_series_varval(operation = "IPC", validate = FALSE), "data.frame")
  expect_s3_class(get_metadata_series_varval(operation = "IPC", det = 2, validate = FALSE), "data.frame")
})
