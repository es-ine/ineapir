# Get data from a specific table

Get data from a specific table

## Usage

``` r
get_data_table(
  idTable = NULL,
  filter = NULL,
  nlast = NULL,
  dateStart = NULL,
  dateEnd = NULL,
  det = 0,
  tip = NULL,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE,
  unnest = FALSE,
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

- nlast:

  (int): number of periods to retrieve. By default it retrieves all
  available periods.

- dateStart:

  (string): the initial date of the requested data. The required format
  is yyyy/mm/dd. Additionally, dateStart can be a vector of dates, where
  each date represents the start date of individual ranges where the end
  date should be found at the same position in the dateEnd vector. If
  dateStart and dateEnd are equal, the specified dates are retrieved. If
  no end date is entered, all dates will be queried, from the
  corresponding start date to the last available period.

- dateEnd:

  (string): the end date of the requested data. The required format is
  yyyy/mm/dd. Additionally, dateEnd can be a vector of dates, where each
  date represents the end date of individual ranges where the initial
  date should be found at the same position in the dateStart vector. The
  length of the dateEnd vector must be less than or equal to the length
  of the dateStart vector.

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- tip:

  (string): set to 'A' for friendly output (e.g. readable dates), set to
  'M' to include metadata or set to 'AM' for both.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- validate:

  (logical): validate the input parameters. A FALSE value means fewer
  API calls. Therefore, it is recommended to set it to FALSE when there
  is no doubt about the validity of the input parameters, including the
  filter.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

- unnest:

  (logical): set to TRUE to obtain a single data frame of data.

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

Data frame with data of a table according to the id specified in the
function

## Examples

``` r
if (FALSE) { # interactive()
# Obtaining the last two periods and filter data
filter <- list("3" = "74", "762" = "304092")
df <-get_data_table(idTable = 76125, nlast = 2, unnest = TRUE,
filter = filter, validate = FALSE)
head(df)

# Get data for an open range date
df <- get_data_table(idTable = 76125, unnest = TRUE, tip= "A",
filter = filter, validate = FALSE,
dateStart = "2025/01/01")
head(df)

# Get data for a single range data
df <- get_data_table(idTable = 76125, unnest = TRUE, tip= "A",
filter = filter, validate = FALSE,
dateStart = "2023/01/01", dateEnd = "2023/05/01")
head(df)

# Get data for specific dates
df <- get_data_table(idTable = 76125, unnest = TRUE, tip= "A",
filter = filter, validate = FALSE,
dateStart = c("2023/01/01","2024/01/01"),
dateEnd = c("2023/01/01","2024/01/01"))
head(df)

# Get data for multiple date ranges
df <- get_data_table(idTable = 76125, unnest = TRUE, tip= "A",
filter = filter, validate = FALSE,
dateStart = c("2023/01/01","2024/01/01"),
dateEnd = c("2023/03/01","2024/03/01"))
head(df)

# Get medatada as well
df <- get_data_table(idTable = 76125, nlast = 2, unnest = TRUE,
filter = filter, validate = FALSE,
metanames = TRUE, metacodes = TRUE, tip = "M")
head(df)
}
```
