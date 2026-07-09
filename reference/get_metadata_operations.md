# Get all available operations

Get all available operations

## Usage

``` r
get_metadata_operations(
  operation = NULL,
  lang = "ES",
  geo = NULL,
  page = 0,
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- operation:

  (string): code of the operation. To obtain a list of available
  operations see `get_metadata_operations()`. If no operation is
  specified then all the operations will be shown

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- geo:

  (int): set to 0 for operations with national data or set to 1 for
  operations with data with a greater level of disaggregation.

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

Data frame with information of the available operations

## Examples

``` r
if (FALSE) { # interactive()
# Get all operations
df <- get_metadata_operations()
head(df)

# Get a specific operation
df <- get_metadata_operations(operation = "IPC", validate = FALSE)
head(df)

# Get operations with territorial disaggregation
df <- get_metadata_operations(geo = 1)
head(df)
}
```
