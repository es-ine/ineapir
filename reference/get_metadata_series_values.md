# Get all the values for a given series

Get all the values for a given series

## Usage

``` r
get_metadata_series_values(
  codSeries = NULL,
  det = 0,
  tip = NULL,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- codSeries:

  (string): code of the series. For further information about codes
  click this
  [link](https://es-ine.github.io/ineapir/articles/identify_codes.html).

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- tip:

  (string): set to 'A' for friendly output (e.g. readable dates), set to
  'M' to include metadata or set to 'AM' for both.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- validate:

  (logical): validate input parameters.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information of the values of a series according to the
code specified in the function

## Examples

``` r
if (FALSE) { # interactive()
# Get metadata of time series with code "IPC290750"
df <- get_metadata_series_values(codSeries = "IPC290750")
head(df)
}
```
