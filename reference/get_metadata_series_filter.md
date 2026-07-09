# Get all the series for a specific operation given a filter

Get all the series for a specific operation given a filter

## Usage

``` r
get_metadata_series_filter(
  operation = NULL,
  filter = NULL,
  periodicity = NULL,
  det = 0,
  tip = NULL,
  lang = "ES",
  page = 1,
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- operation:

  (string): code of the operation. To obtain a list of available
  operations see
  [`get_metadata_operations()`](https://inedifusion.github.io/ineapir/reference/get_metadata_operations.md).

- filter:

  (list): list of variables and values.

  ### Filtering data from series

  When we request data from series there is the possibility of filtering
  data on the fly using metadata information about the variables and
  their values that define the series. To get variables for a given
  operation see
  [`get_metadata_variables()`](https://inedifusion.github.io/ineapir/reference/get_metadata_variables.md)
  and to get values for a specific variable see
  [`get_metadata_values()`](https://inedifusion.github.io/ineapir/reference/get_metadata_values.md).
  See also
  [`get_metadata_series_varval()`](https://inedifusion.github.io/ineapir/reference/get_metadata_series_varval.md)
  to get all the values at once.

  #### Filter format

  The format is
  `list(id_variable1 = id_value1, id_variable2 = id_value2)`. Besides:

  - A variable can take more than one value:
    `list(id_variable1 = c(id_value11, id_value12), id_variable2 = id_value2)`.

  - A variable can take a empty character "" to get all its possible
    values: `list(id_variable1 = id_value1, id_variable2 = "")`.

  #### Using shortcuts

  Additionally, shortcuts can be used to filter. They simplify the
  filtering approach by using standardized names for variable IDs and
  therefore simplify their use. The format is:
  `list(shortcut_variable1 = name1, shortcut_variable2 = name2)`.
  Besides, the *values* wrapper can also be used:
  `list(values = c(name1, name2)`. To see a list of all available
  shortcuts, see
  [`get_filter_shortcuts()`](https://inedifusion.github.io/ineapir/reference/get_filter_shortcuts.md)
  function. Let’s also remark that for better performance is recommended
  to use numeric codes.

- periodicity:

  (int): id of the periodicity of the series. Common periodicities: 1
  (monthly), 3 (quarterly), 6 (bi-annual), 12 (annual). To obtain a list
  of periodicities see
  [`get_metadata_periodicity()`](https://inedifusion.github.io/ineapir/reference/get_metadata_periodicity.md).

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- tip:

  (string): set to 'A' for friendly output (e.g. readable dates), set to
  'M' to include metadata or set to 'AM' for both.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- page:

  (int): page number. The retrieved result of the query is paginated.
  Default value is set to 1.

- validate:

  (logical): validate input parameters. A FALSE value means fewer API
  calls. Therefore, it is recommended to set it to FALSE when there is
  no doubt about the validity of the input parameters, including the
  filter.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information of the series according to the operation and
filter specified in the function

## Examples

``` r
if (FALSE) { # interactive()
# Get time series from "IPC" operation that include the filter variables
# and values
df <- get_metadata_series_filter(operation = "IPC", periodicity = 1,
filter = list("115"= "29", "3" = "84", "762" = ""), validate = FALSE)
head(df)
}
```
