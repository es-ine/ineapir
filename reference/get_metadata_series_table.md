# Get all the series for a given table

Get all the series for a given table

## Usage

``` r
get_metadata_series_table(
  idTable = NULL,
  filter = NULL,
  det = 0,
  tip = NULL,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE,
  metanames = FALSE,
  metacodes = FALSE
)
```

## Arguments

- idTable:

  (int): id of the table. For further information about ids click this
  [link](https://es-ine.github.io/ineapir/articles/identify_codes.html).

- filter:

  (list): list of variables and values.

  ### Filtering data from tables

  When we request data from tables there is the possibility of filtering
  data on the fly using metadata information about the variables and
  their values that define the series. See
  [`get_metadata_table_varval()`](https://inedifusion.github.io/ineapir/reference/get_metadata_table_varval.md)
  to get all the values at once. There are different approaches to build
  the filter depending on the table type.

  #### Case one: tempus table

  [URL example](https://www.ine.es/jaxiT3/Tabla.htm?t=76125). For a
  tempus table the filter is based on ids. The format is
  `list(id_variable1 = id_value1, id_variable2 = id_value2)`. Besides:

  - A variable can take more than one value:
    `list(id_variable1 = c(id_value11, id_value12), id_variable2 = id_value2)`.

  - A variable can take a empty character "" to get all its possible
    values: `list(id_variable1 = id_value1, id_variable2 = "")`.

  #### Case two: px tables

  [URL
  example](https://www.ine.es/jaxi/Tabla.htm?path=/t20/e245/p08/l0/&file=01001.px).
  For a px table the filter is based on codes. The format is
  `list(cod_variable1 = cod_value1, cod_variable2 = cod_value2)`.
  Besides:

  - A variable can take more than one value:
    `list(cod_variable1 = c(cod_value11, cod_value12), id_variable2 = cod_value2)`.

  - A variable can take a empty character "" to get all its possible
    values: `list(cod_variable1 = cod_value1, cod_variable2 = "")`.

  #### Case three: tpx table

  [URL example](https://www.ine.es/jaxi/Tabla.htm?tpx=33387&L=0). For a
  tpx table the filter is based on codes. The format is
  `list(cod_variable1 = cod_value1, cod_variable2 = cod_value2)`.
  Besides:

  - A variable can take more than one value:
    `list(cod_variable1 = c(cod_value11, cod_value12), id_variable2 = cod_value2)`.

  - A variable can take a empty character "" to get all its possible
    values: `list(cod_variable1 = cod_value1, cod_variable2 = "")`.

  [URL example](https://www.ine.es/jaxi/Tabla.htm?tpx=52056&L=0). There
  are tpx tables that contain variable ids and value ids. In this case,
  we can use the ids instead of the codes to build the filter. To do
  this we add the alias *~id* at the end of each id:
  `list(id_variable1~id = id_value1~id, id_variable2~id = id_value2~id)`.

  #### Using shortcuts

  Additionally, shortcuts can be used to filter. They simplify the
  filtering approach by using standardized names for variable IDs and
  therefore simplify their use. The format for a tempus table is:
  `list(shortcut_variable1 = name1, shortcut_variable2 = name2)`.
  However, for px and tpx tables the format is:
  `list(values = c(name1, name2)`. The *values* wrapper can also be used
  with tempus tables. To see a list of all available shortcuts, see
  [`get_filter_shortcuts()`](https://inedifusion.github.io/ineapir/reference/get_filter_shortcuts.md)
  function. Let’s also remark that for better performance is recommended
  to use numeric ids for tempus tables and alphanumeric codes for px and
  tpx tables.

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- tip:

  (string): set to 'A' for friendly output (e.g. readable dates), set to
  'M' to include metadata or set to 'AM' for both.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- validate:

  (logical): validate input parameters. A FALSE value means fewer API
  calls. Therefore, it is recommended to set it to FALSE when there is
  no doubt about the validity of the input parameters, including the
  filter.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

- metanames:

  (logical): set to TRUE to extract the name of the values that defined
  the table. The names are extracted from the metadata information (it
  is mandatory to include 'M' in the tip parameter). Several columns are
  created corresponding to the values of the different variables.

- metacodes:

  (logical): set to TRUE to extract the codes and ids of the values that
  defined the table. The codes and ids are extracted from the metadata
  information (it is mandatory to include 'M' in the tip parameter).
  Several columns are created corresponding to the values of the
  different variables.

## Value

Data frame with information of the series for a given table.

## Examples

``` r
if (FALSE) { # interactive()
# Get time series without data from table with identification code "76125"
filter <- list("3" = "83")
df <- get_metadata_series_table(idTable = 76125, validate = FALSE,
filter = filter)
head(df)

# Get metadata as well
df <- get_metadata_series_table(idTable = 76125, validate = FALSE,
filter = filter, metanames = TRUE, metacodes = TRUE, tip = "M")
head(df)
}
```
