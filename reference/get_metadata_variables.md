# Get all available variables

Get all available variables

## Usage

``` r
get_metadata_variables(
  operation = NULL,
  lang = "ES",
  det = 0,
  page = 0,
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- operation:

  (string): Code of the operation. Provide code to get all the variables
  for the given operation. To obtain a list of available operations see
  [`get_metadata_operations()`](https://inedifusion.github.io/ineapir/reference/get_metadata_operations.md).
  If no operation is specified then all the variables will be shown.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

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

Data frame with information of the available variables

## Examples

``` r
if (FALSE) { # interactive()
# Gel all variables
df <- get_metadata_variables()
head(df)

# Get variables for a specific operation
df <- get_metadata_variables(operation = "IPC", validate = FALSE)
head(df)
}
```
