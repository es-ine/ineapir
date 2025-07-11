test_that("test get_metadata_operations", {
  skip_on_cran()
  expect_s3_class(get_metadata_operations(), "data.frame")
  expect_type(get_metadata_operations(operation = "IPC", validate = FALSE), "list")
})

test_that("test get_metadata_variables", {
  skip_on_cran()
  expect_s3_class(get_metadata_variables(), "data.frame")
  expect_s3_class(get_metadata_variables(operation = "IPC", validate = FALSE), "data.frame")
})

test_that("test get_metadata_values", {
  skip_on_cran()
  expect_s3_class(get_metadata_values(variable = 70, validate = FALSE), "data.frame")
  expect_s3_class(get_metadata_values(variable = 70, operation = "IPC", validate = FALSE), "data.frame")
  expect_s3_class(get_metadata_values(variable = 70, value = 8997, validate = FALSE), "data.frame")
})

test_that("test get_metadata_publications", {
  skip_on_cran()
  expect_s3_class(get_metadata_publications(), "data.frame")
  expect_s3_class(get_metadata_publications(operation = "IPC", validate = FALSE), "data.frame")
})

test_that("test get_metadata_publication_dates", {
  skip_on_cran()
  expect_s3_class(get_metadata_publication_dates(publication = 8, validate = FALSE), "data.frame")
})

test_that("test get_metadata_periodicity", {
  skip_on_cran()
  expect_s3_class(get_metadata_periodicity(), "data.frame")
  expect_s3_class(get_metadata_periodicity(operation = "IPC", validate = FALSE), "data.frame")
})

test_that("test get_filter_shortcuts", {
  expect_s3_class(get_filter_shortcuts(), "data.frame")
  expect_s3_class(get_filter_shortcuts(lang = "EN"), "data.frame")
})

test_that("test get_metadata_classifications", {
  skip_on_cran()
  expect_s3_class(get_metadata_classifications(), "data.frame")
  expect_s3_class(get_metadata_classifications(operation = "IPC", validate = FALSE), "data.frame")
})
