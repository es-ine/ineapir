test_that("test unnest", {
  skip_on_cran()
  expect_s3_class(get_data_series(codSeries = "IPC251856", unnest = TRUE, nlast = 5), "data.frame")
})

test_that("test det", {
  skip_on_cran()
  expect_s3_class(get_data_series(codSeries = "IPC251856", unnest = TRUE, nlast = 5, det = 2), "data.frame")
})

test_that("test date", {
  skip_on_cran()
  expect_s3_class(get_data_series(codSeries = "IPC251856", unnest = TRUE, nlast = 5,
                                  dateStart = c("2023/01/01","2024/01/01"),
                                  dateEnd = c("2023/01/01","2024/01/01")), "data.frame")
})

test_that("test filter", {
  skip_on_cran()
  expect_s3_class(get_data_series_filter(operation = "IPC", periodicity = 1, nlast = 5, unnest = TRUE, validate = FALSE,
                                         filter = list("115"= "28", "3" = "84", "762" = "304092")), "data.frame")

})
