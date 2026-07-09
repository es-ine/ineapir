# Get all tables for a given operation

Get all tables for a given operation

## Usage

``` r
get_metadata_tables_operation(
  operation = NULL,
  det = 0,
  tip = NULL,
  geo = NULL,
  lang = "ES",
  page = 0,
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

- geo:

  (int): set to 0 for national tables or set to 1 for tables with a
  greater level of disaggregation.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English

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

Data frame with information of the available tables according to the
operation specified in the function

## Examples

``` r
if (FALSE) { # interactive()
# Get all the tables of the "IPC" operation
df <- get_metadata_tables_operation(operation = "IPC", validate = FALSE)
head(df)
}
```
