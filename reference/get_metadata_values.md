# Get all values for a specific variable

Get all values for a specific variable

## Usage

``` r
get_metadata_values(
  operation = NULL,
  variable = NULL,
  value = NULL,
  det = 0,
  lang = "ES",
  page = 0,
  classification = NULL,
  validate = TRUE,
  verbose = FALSE,
  hierarchy = NULL,
  filter = NULL
)
```

## Arguments

- operation:

  (string): code of the operation. Provide code to get all the values
  for the given operation. To obtain a list of available operations see
  [`get_metadata_operations()`](https://inedifusion.github.io/ineapir/reference/get_metadata_operations.md).

- variable:

  (int): id of a variable. To obtain a list of available variables see
  [`get_metadata_variables()`](https://inedifusion.github.io/ineapir/reference/get_metadata_variables.md).

- value:

  (int): id of a value. If an id value is specified, the children of the
  value are requested. To obtain a list of available values for a
  variable use `get_metadata_values(variable = id_variable)`.

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- page:

  (int): page number. The retrieved result of the query is paginated
  (page=0 retrieves all pages).

- classification:

  (int): id of a classification. To obtain a list of available
  classifications see
  [`get_metadata_classifications()`](https://inedifusion.github.io/ineapir/reference/get_metadata_classifications.md).

- validate:

  (logical): validate input parameters. A FALSE value means fewer API
  calls.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

- hierarchy:

  (int): depth of the hierarchy tree.

- filter:

  (list): list of variables and values. When we request the hierarchy
  tree there is the possibility of filtering using metadata information
  about the variables and their values that define the series. The
  format is `list(id_variable1 = id_value1, id_variable2 = id_value2)`.
  Besides:

  - A variable can take more than one value:
    `list(id_variable1 = c(id_value11, id_value12), id_variable2 = id_value2)`.

  - A variable can take a empty character "" to get all its possible
    values: `list(id_variable1 = id_value1, id_variable2 = "")`.

## Value

Data frame with information of the available values for the variable
specified in the function

## Examples

``` r
if (FALSE) { # interactive()
# Get the values of the variable "115"
df <- get_metadata_values(variable = 115)
head(df)

# Get the values of a variable for a specific operation
df <- get_metadata_values(operation = "IPC", variable = 115, validate = FALSE)
head(df)

# Get the children of a value (provinces of Galicia)
# Variable: Autonomous communities (id=70)
# Value: Galicia (id=9008)
df <- get_metadata_values(variable = 70, value = 9008)
head(df)
}
```
