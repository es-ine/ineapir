# Get metadata information about the variables and values of series for a given operation

Get metadata information about the variables and values of series for a
given operation

## Usage

``` r
get_metadata_series_varval(
  operation = NULL,
  lang = "ES",
  det = 0,
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- operation:

  (string): code of the operation. To obtain a list of available
  operations see
  [`get_metadata_operations()`](https://inedifusion.github.io/ineapir/reference/get_metadata_operations.md).

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- validate:

  (logical): validate input parameters. A FALSE value means fewer API
  calls.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information about the variables and values that define
the series according to the operation specified in the function

## Examples

``` r
if (FALSE) { # interactive()
# Get metadata information of time series from "IPC" operation
df <- get_metadata_series_varval(operation = "IPC", validate = FALSE)
head(df)
}
```
