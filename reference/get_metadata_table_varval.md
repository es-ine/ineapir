# Get metadata information about the variables and values for a given table

Get metadata information about the variables and values for a given
table

## Usage

``` r
get_metadata_table_varval(
  idTable = NULL,
  det = 0,
  filter = NULL,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- idTable:

  (int): id of the table. For further information about ids click this
  [link](https://es-ine.github.io/ineapir/articles/identify_codes.html).

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- filter:

  (list): list of variables and values.

  ### Filtering data from tables

  When we request data from tables there is the possibility of filtering
  data on the fly using metadata information about the variables and
  their values that define the series. There are different approaches to
  build the filter depending on the table type.

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

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- validate:

  (logical): validate input parameters.A FALSE value means fewer API
  calls. Therefore, it is recommended to set it to FALSE when there is
  no doubt about the validity of the input parameters, including the
  filter.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information about the variables and values that define a
table according to the table specified in the function

## Examples

``` r
if (FALSE) { # interactive()
# Get all the variable and values of the table with identification code "76125"
df <- get_metadata_table_varval(idTable = 76125)
head(df)

# Filter variables and values
df <- get_metadata_table_varval(idTable = 52056,
filter = list(NAC = "00"), validate = FALSE)
head(df)
}
```
