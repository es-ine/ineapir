# Changelog

## ineapir 0.2.6

CRAN release: 2026-07-08

- Debugged error in the date argument of the get_data_table() function.
- Documentation update.

## ineapir 0.2.5

CRAN release: 2025-09-15

- CRAN resubmission
- Added a web reference for the API to the description of the
  DESCRIPTION file

## ineapir 0.2.4

- Initial CRAN submission

## ineapir 0.2.3

- Function usage examples have been reorganized.
- Improvements in compatibility for future versions of the API.

## ineapir 0.2.2

- Added testing of the package
- Package description updated

## ineapir 0.2.1

- Debugged error in the filter argument of the get_metadata values()
  function.

## ineapir 0.2.0

- Added the function
  [`get_metadata_classifications()`](https://inedifusion.github.io/ineapir/reference/get_metadata_classifications.md).
- Added new funcionality to
  [`get_metadata_values()`](https://inedifusion.github.io/ineapir/reference/get_metadata_values.md):
  request values hierarchy trees.
- Added the arguments `value`, `classification`, `hierarchy` and
  `filter` to
  [`get_metadata_values()`](https://inedifusion.github.io/ineapir/reference/get_metadata_values.md).

## ineapir 0.1.0

- Added the arguments `dateStart` y `dateEnd` to
  [`get_data_table()`](https://inedifusion.github.io/ineapir/reference/get_data_table.md)
  and allow multiple range of dates.
- New filter funcionality: if a minus sign id added to a value, the
  value will be excluded from the filter.
- Added the argument `det`to
  [`get_metadata_table_varval()`](https://inedifusion.github.io/ineapir/reference/get_metadata_table_varval.md).
- Added the argument `det`to
  [`get_metadata_variables()`](https://inedifusion.github.io/ineapir/reference/get_metadata_variables.md).

## ineapir 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
