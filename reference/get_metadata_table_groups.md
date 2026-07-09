# Get all groups for a specific a table

Get all groups for a specific a table

## Usage

``` r
get_metadata_table_groups(
  idTable = NULL,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- idTable:

  (int): id of the table. For further information about ids click this
  [link](https://es-ine.github.io/ineapir/articles/identify_codes.html).

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- validate:

  (logical): validate input parameters.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information of the groups according to the table
specified in the function

## Examples

``` r
if (FALSE) { # interactive()
# Get the groups of the table with identification code "76125"
df <- get_metadata_table_groups(idTable = 76125)
head(df)
}
```
