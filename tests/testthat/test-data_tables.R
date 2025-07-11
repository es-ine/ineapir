test_that("test unnest", {
  skip_on_cran()
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2), "data.frame")
})


test_that("test metadata", {
  skip_on_cran()
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2, tip = "M",
                                          metanames = TRUE, metacodes = TRUE), "data.frame")

})

test_that("test filter", {
  skip_on_cran()
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2,
                                          filter <- list("3" = "74", "762" = "304092")), "data.frame")
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2, validate = FALSE,
                                          filter <- list("3" = "74", "762" = "304092")), "data.frame")
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2, validate = FALSE,
                                          filter <- list("3" = "-74", "762" = "304092")), "data.frame")
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2, validate = FALSE,
                                          filter <- list(values = c("variación anual" , "índice general"))), "data.frame")
})

test_that("test det", {
  skip_on_cran()
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2, det = 2), "data.frame")
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2, tip = "M", det = 2,
                                          metanames = TRUE, metacodes = TRUE), "data.frame")
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2,
                                          validate = FALSE, det = 2,
                                          filter <- list("3" = "-74", "762" = "304092")), "data.frame")
})

test_that("test date", {
  skip_on_cran()
  expect_s3_class(get_data_table(idTable = 50902, unnest = TRUE, nlast = 2, validate = FALSE,
                                          filter <- list("3" = "74", "762" = "304092"),
                                          dateStart = c("2023/01/01","2024/01/01"),
                                          dateEnd = c("2023/01/01","2024/01/01")), "data.frame")
})
