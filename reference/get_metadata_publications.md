# Get all publications

Get all publications

## Usage

``` r
get_metadata_publications(
  operation = NULL,
  det = 0,
  lang = "ES",
  page = 0,
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- operation:

  (string): code of the operation. Provide code to get all the
  publications for the given operation. To obtain a list of available
  operations see
  [`get_metadata_operations()`](https://inedifusion.github.io/ineapir/reference/get_metadata_operations.md).
  If no operation is specified then all the publications will be shown.

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- page:

  (int): page number. The retrieved result of the query is paginated
  (page=0 retrieves all pages).

- validate:

  (logical): validate input parameters. A FALSE value means fewer API
  calls.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information about publications

## Examples

``` r
if (FALSE) { # interactive()
# Get all publications
df <- get_metadata_publications()
head(df)

# Get publications for a specific operation
df <- get_metadata_publications(operation = "IPC", validate = FALSE)
head(df)
}
```
