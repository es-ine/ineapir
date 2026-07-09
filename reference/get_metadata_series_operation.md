# Get all the series for a specific operation

Get all the series for a specific operation

## Usage

``` r
get_metadata_series_operation(
  operation = NULL,
  det = 0,
  tip = NULL,
  lang = "ES",
  page = 1,
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- operation:

  (string): code of the operation. To obtain a list of available
  operations see
  [`get_metadata_operations()`](https://inedifusion.github.io/ineapir/reference/get_metadata_operations.md).

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- tip:

  (string): set to 'A' for friendly output (e.g. readable dates), set to
  'M' to include metadata or set to 'AM' for both.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- page:

  (int): page number. The retrieved result of the query is paginated.
  Default value is set to 1.

- validate:

  (logical): validate input parameters. A FALSE value means fewer API
  calls.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information of the series belonging to an operation.

## Examples

``` r
if (FALSE) { # interactive()
# Get metadata of time series from "IPC" operation
# Retrieve page 1
df <- get_metadata_series_operation(operation = "IPC", validate = FALSE)
nrow(df)

# Retrieve page 2
df <- get_metadata_series_operation(operation = "IPC", validate = FALSE,
page = 2)
nrow(df)
}
```
