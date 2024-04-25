#' Get all tables for a given operation
#'
#' @param operation (string): code of the operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param geo (int): set to 0 for national tables or set to 1 for tables with
#' a greater level of disaggregation.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages).
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the available tables according to the operation specified in the function
#' @export
#'
#' @examples \dontrun{
#' get_metadata_tables_operation(operation = "IPC")
#' }
#'
get_metadata_tables_operation <- function(operation = NULL, det = 0, tip = NULL, geo = NULL, lang = "ES", page = 0, validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "TABLAS_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
  parameters <- append(parameters, list(geo = geo))
  parameters <- append(parameters, list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data_all_pages(url, request)

  return(data)
}

#' Get all groups for a specific a table
#'
#' @param idTable (int): id of the table. For further information about
#' ids click this [link](https://uvima.github.io/ineapir/articles/identify_codes.html).
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the groups according to the table specified in the function
#' @export
#'
#' @examples \dontrun{
#' get_metadata_table_groups(idTable = 50902)
#' }
#'
get_metadata_table_groups <- function(idTable = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "GRUPOS_TABLA"))
  definition <- append(definition, list(input = idTable))
  definition <- append(definition, list(tag = "idTable"))

  # List of parameters to call the API
  parameters <- list()

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

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

#' Get all values for a specific table group
#'
#' @param idTable (int): id of the table. For further information about
#' ids click this [link](https://uvima.github.io/ineapir/articles/identify_codes.html).
#' @param idGroup (int): id of the group of variables. To get all groups
#' for a specific table see [get_metadata_table_groups()].
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the values of a table group according to the table and group specified in the function
#' @export
#'
#' @examples \dontrun{
#' get_metadata_table_values(idTable = 50902, idGroup = 110889)
#' }
#'
get_metadata_table_values <- function(idTable = NULL, idGroup = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "VALORES_GRUPOSTABLA"))
  definition <- append(definition, list(input = list(idTable = idTable, idGroup = idGroup)))
  definition <- append(definition, list(tag = "idTable_idGroup"))

  # List of parameters to call the API
  parameters <- list()

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

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

#' Get the operation for a given table
#'
#' @param idTable (int): id of the table. For further information about
#' ids click this [link](https://uvima.github.io/ineapir/articles/identify_codes.html).
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the operation according to the table specified in the function
#' @export
#'
#' @examples \dontrun{
#' get_metadata_operation_table(idTable = 50902)
#' }
#'
get_metadata_operation_table <- function(idTable = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "OPERACIONES_TABLA"))
  definition <- append(definition, list(input = idTable))
  definition <- append(definition, list(tag = "idTable"))

  # List of parameters to call the API
  parameters <- list()

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

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

#' Get metadata information about the variables and values for a given table
#'
#' @param idTable (int): id of the table. For further information about
#' ids click this [link](https://uvima.github.io/ineapir/articles/identify_codes.html).
#' @param filter (list): list of variables and values.
#' ### Filtering data from tables
#' When we request data from tables there is the possibility of filtering data
#' on the fly using metadata information about the variables and their values
#' that define the series. There are different approaches to build the filter depending on the table type.
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
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters.A FALSE value means fewer API calls.
#' Therefore, it is recommended to set it to FALSE when there is no doubt about the validity of the input parameters, including the filter.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information about the variables and values that define a table according to the table specified in the function
#' @export
#'
#' @examples \dontrun{
#' get_metadata_table_varval(idTable = 50902)
#' get_metadata_table_varval(idTable = 52056, filter = list(NAC = "00"))
#' }
#'
get_metadata_table_varval <- function(idTable = NULL, filter = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # Get the metadata information of the table
  df <- get_metadata_variable_values_table(idTable, filter, verbose, validate, lang)

  return(df$values)
}
