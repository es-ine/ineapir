# Get all values for a specific table group

Get all values for a specific table group

## Usage

``` r
get_metadata_table_values(
  idTable = NULL,
  idGroup = NULL,
  det = 0,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- idTable:

  (int): id of the table. For further information about ids click this
  [link](https://es-ine.github.io/ineapir/articles/identify_codes.html).

- idGroup:

  (int): id of the group of variables. To get all groups for a specific
  table see
  [`get_metadata_table_groups()`](https://inedifusion.github.io/ineapir/reference/get_metadata_table_groups.md).

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

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

Data frame with information of the values of a table group according to
the table and group specified in the function

## Examples

``` r
if (FALSE) { # interactive()
# Get the values of the group "155577" of the table with identification
# code "76125"
df <- get_metadata_table_values(idTable = 76125, idGroup = 155577, validate = FALSE)
head(df)
}
```
