test_that("test get_metadata_tables_operation", {
  skip_on_cran()
  expect_s3_class(get_metadata_tables_operation(operation = "IPC", validate = FALSE), "data.frame")
})

test_that("test get_metadata_table_groups", {
  skip_on_cran()
  expect_s3_class(get_metadata_table_groups(idTable = 50902), "data.frame")
})

test_that("test get_metadata_table_values", {
  skip_on_cran()
  expect_s3_class(get_metadata_table_values(idTable = 50902, idGroup = 110889, validate = FALSE), "data.frame")
  expect_s3_class(get_metadata_table_values(idTable = 50902, idGroup = 110889, det = 2, validate = FALSE), "data.frame")
})

test_that("test get_metadata_operation_table", {
  skip_on_cran()
  expect_s3_class(get_metadata_operation_table(idTable = 50902), "data.frame")
})

test_that("test gget_metadata_table_varval", {
  skip_on_cran()
  expect_s3_class(get_metadata_table_varval(idTable = 50902), "data.frame")
  expect_s3_class(get_metadata_table_varval(idTable = 50902, det = 2), "data.frame")
  expect_s3_class(get_metadata_table_varval(idTable = 50902, validate = FALSE,
                                            filter = list("3" = "74")), "data.frame")
})

