test_that("test get_metadata_tables_operation", {
  skip_on_cran()
  expect_s3_class(get_metadata_tables_operation(operation = "IPC", validate = FALSE), "data.frame")
})

test_that("test get_metadata_table_groups", {
  skip_on_cran()
  expect_s3_class(get_metadata_table_groups(idTable = 76125), "data.frame")
})

test_that("test get_metadata_table_values", {
  skip_on_cran()
  expect_s3_class(get_metadata_table_values(idTable = 76125, idGroup = 155577, validate = FALSE), "data.frame")
  expect_s3_class(get_metadata_table_values(idTable = 76125, idGroup = 155577, det = 2, validate = FALSE), "data.frame")
})

test_that("test get_metadata_operation_table", {
  skip_on_cran()
  expect_s3_class(get_metadata_operation_table(idTable = 76125), "data.frame")
})

test_that("test get_metadata_table_varval", {
  skip_on_cran()
  expect_s3_class(get_metadata_table_varval(idTable = 76125), "data.frame")
  expect_s3_class(get_metadata_table_varval(idTable = 76125, det = 2), "data.frame")
  expect_s3_class(get_metadata_table_varval(idTable = 76125, validate = FALSE,
                                            filter = list("3" = "74")), "data.frame")
})

