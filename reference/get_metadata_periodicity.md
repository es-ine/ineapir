# Get all available periodicities

Get all available periodicities

## Usage

``` r
get_metadata_periodicity(
  operation = NULL,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- operation:

  (string): Code of the operation. Provide code to get all the
  periodicities for the given operation. To obtain a list of available
  operations see
  [`get_metadata_operations()`](https://inedifusion.github.io/ineapir/reference/get_metadata_operations.md).
  If no operation is specified then all the periodicities will be shown.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- validate:

  (logical): validate input parameters. A FALSE value means fewer API
  calls.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information of the available periodicities

## Examples

``` r
if (FALSE) { # interactive()
# Get all periodicities
df <- get_metadata_periodicity()
head(df)

# Get periodicities for a specific operation
df <- get_metadata_periodicity(operation = "IPC", validate = FALSE)
head(df)
}
```
