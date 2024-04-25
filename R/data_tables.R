#' Get data from a specific table
#'
#' @param idTable (int): id of the table. For further information about
#' ids click this [link](https://uvima.github.io/ineapir/articles/identify_codes.html).
#' @param filter (list): list of variables and values.
#' ### Filtering data from tables
#' When we request data from tables there is the possibility of filtering data
#' on the fly using metadata information about the variables and their values
#' that define the series. See [get_metadata_table_varval()] to get all the values at once.
#' There are different approaches to build the filter depending on the table type.
#'
#' #### Case one: tempus table
#' [URL example](https://www.ine.es/jaxiT3/Tabla.htm?t=50902).
#' For a tempus table the filter is based on ids. The format is `list(id_variable1 = id_value1, id_variable2 = id_value2)`.
#' Besides:
#' - A variable can take more than one value: `list(id_variable1 = c(id_value11, id_value12), id_variable2 = id_value2)`.
#' - A variable can take a empty character "" to get all its possible values: `list(id_variable1 = id_value1, id_variable2 = "")`.
#'
#' #### Case two: px tables
#' [URL example](https://www.ine.es/jaxi/Tabla.htm?path=/t20/e245/p08/l0/&file=01001.px).
#' For a px table the filter is based on codes. The format is `list(cod_variable1 = cod_value1, cod_variable2 = cod_value2)`.
#' Besides:
#' - A variable can take more than one value: `list(cod_variable1 = c(cod_value11, cod_value12), id_variable2 = cod_value2)`.
#' - A variable can take a empty character "" to get all its possible values: `list(cod_variable1 = cod_value1, cod_variable2 = "")`.

#' #### Case three: tpx table
#' [URL example](https://www.ine.es/jaxi/Tabla.htm?tpx=33387&L=0).
#' For a tpx table the filter is based on codes. The format is `list(cod_variable1 = cod_value1, cod_variable2 = cod_value2)`.
#' Besides:
#' - A variable can take more than one value: `list(cod_variable1 = c(cod_value11, cod_value12), id_variable2 = cod_value2)`.
#' - A variable can take a empty character "" to get all its possible values: `list(cod_variable1 = cod_value1, cod_variable2 = "")`.
#'
#' [URL example](https://www.ine.es/jaxi/Tabla.htm?tpx=52056&L=0).
#' There are tpx tables that contain variable ids and value ids. In this case,
#' we can use the ids instead of the codes to build the filter. To do this we add
#' the alias *~id* at the end of each id: `list(id_variable1~id = id_value1~id, id_variable2~id = id_value2~id)`.
#'
#' #### Using shortcuts
#' Additionally, shortcuts can be used to filter. They simplify the filtering
#' approach by using standardized names for variable IDs and therefore simplify
#' their use. The format for a tempus table is:
#' `list(shortcut_variable1 = name1, shortcut_variable2 = name2)`. However,
#' for px and tpx tables the format is: `list(values = c(name1, name2)`. The
#' *values* wrapper can also be used with tempus tables.
#' To see a list of all available shortcuts, see [get_filter_shortcuts()] function.
#' Let’s also remark that for better performance is recommended to use numeric ids
#' for tempus tables and alphanumeric codes for px and tpx tables.
#' @param nlast (int): number of periods to retrieve. By default it retrieves all available periods.
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#'  set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A FALSE value means fewer API calls.
#' Therefore, it is recommended to set it to FALSE when there is no doubt about the validity of the input parameters, including the filter.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#' @param unnest (logical): set to TRUE to obtain a single data frame of data.
#' @param metanames (logical): set to TRUE to extract the name of the values that defined the table.
#' The names are extracted from the metadata information (it is mandatory to include 'M' in the tip parameter).
#' Several columns are created corresponding to the values of the different variables.
#' @param metacodes (logical): set to TRUE to extract the codes and ids of the values that defined the table.
#' The codes and ids are extracted from the metadata information (it is mandatory to include 'M' in the tip parameter).
#' Several columns are created corresponding to the values of the different variables.
#'
#' @return Data frame with data of a table according to the id specified in the function
#' @export
#'
#' @examples \dontrun{
#' get_data_table(idTable = 50902)
#' get_data_table(idTable = 50902, nlast = 2, unnest = TRUE, metanames = TRUE,
#'                metacodes = TRUE, tip = "M")
#' get_data_table(idTable = 8105, filter = list("18"="454"), verbose = TRUE)
#' get_data_table(idTable = 33387,
#'                filter = list(tipodematerial = c("extraccionnacional",
#'                                                 "2mineralesmetalicosmineralenbruto")))
#' get_data_table(idTable = "t20/e245/p08/l0/01001.px",
#'                filter = list(edad3gruposdeedad = "015anos",
#'                              sexo = c("mujeres", "hombres")))
#' }
#'
get_data_table <- function(idTable = NULL, filter = NULL, nlast = NULL, det = NULL, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE, unnest = FALSE, metanames = FALSE, metacodes = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "DATOS_TABLA"))
  definition <- append(definition, list(input = idTable))
  definition <- append(definition, list(tag = "idTable"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, if(is.null(filter)) list(filter = filter) else list(filter = list(idTable = idTable, filter = filter)))
  parameters <- append(parameters, list(nult = nlast))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose, unnest = unnest, metanames = metanames, metacodes = metacodes)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request)

  return(data)
}
