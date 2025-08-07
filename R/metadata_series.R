#' Get information for a specific series
#'
#' @param codSeries (string): code of the series. For further information about
#' codes click this [link](https://uvima.github.io/ineapir/articles/identify_codes.html).
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of a series according to the code specified in the function
#'
#' @examplesIf interactive()
#' # Get information of time series with code "IPC206449"
#' df <- get_metadata_series(codSeries = "IPC206449")
#' head(df)
#'
#' @export
get_metadata_series <- function(codSeries = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIE"))
  definition <- append(definition, list(input = codSeries))
  definition <- append(definition, list(tag = "codSeries"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

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

#' Get all the series for a specific operation
#'
#' @param operation (string): code of the operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param page (int): page number. The retrieved result of the query is paginated. Default value is set to 1.
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the series belonging to an operation.
#'
#' @examplesIf interactive()
#' # Get metadata of time series from "IPC" operation
#' # Retrieve page 1
#' df <- get_metadata_series_operation(operation = "IPC", validate = FALSE)
#' nrow(df)
#'
#' # Retrieve page 2
#' df <- get_metadata_series_operation(operation = "IPC", validate = FALSE,
#' page = 2)
#' nrow(df)
#'
#' @export
get_metadata_series_operation <- function(operation = NULL, det = 0, tip = NULL, lang = "ES", page = 1 ,validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIES_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
  parameters <- append(parameters, if(page == 0) list(page = 1) else list(page = page))

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

#' Get all the values for a given series
#'
#' @param codSeries (string): code of the series. For further information about
#' codes click this [link](https://uvima.github.io/ineapir/articles/identify_codes.html).
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the values of a series according to the code specified in the function
#'
#' @examplesIf interactive()
#' # Get metadata of time series with code "IPC206449"
#' df <- get_metadata_series_values(codSeries = "IPC206449")
#' head(df)
#'
#' @export
get_metadata_series_values <- function(codSeries = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "VALORES_SERIE"))
  definition <- append(definition, list(input = codSeries))
  definition <- append(definition, list(tag = "codSeries"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

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

#' Get all the series for a given table
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
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' Therefore, it is recommended to set it to FALSE when there is no doubt about the validity of the input parameters, including the filter.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#' @param metanames (logical): set to TRUE to extract the name of the values that defined the table.
#' The names are extracted from the metadata information (it is mandatory to include 'M' in the tip parameter).
#' Several columns are created corresponding to the values of the different variables.
#' @param metacodes (logical): set to TRUE to extract the codes and ids of the values that defined the table.
#' The codes and ids are extracted from the metadata information (it is mandatory to include 'M' in the tip parameter).
#' Several columns are created corresponding to the values of the different variables.
#'
#' @return Data frame with information of the series for a given table.
#'
#' @examplesIf interactive()
#' # Get time series without data from table with identification code "50902"
#' filter <- list("3" = "83")
#' df <- get_metadata_series_table(idTable = 50902, validate = FALSE,
#' filter = filter)
#' head(df)
#'
#' # Get metadata as well
#' df <- get_metadata_series_table(idTable = 50902, validate = FALSE,
#' filter = filter, metanames = TRUE, metacodes = TRUE, tip = "M")
#' head(df)
#'
#' @export
get_metadata_series_table <- function(idTable = NULL, filter = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE, metanames = FALSE, metacodes = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIES_TABLA"))
  definition <- append(definition, list(input = idTable))
  definition <- append(definition, list(tag = "idTable"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, if(is.null(filter)) list(filter = filter) else list(filter = list(idTable = idTable, filter = filter)))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose, metanames = metanames, metacodes = metacodes)

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

#' Get all the series for a specific operation given a filter
#'
#' @param operation (string): code of the operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' @param filter (list): list of variables and values.
#' ### Filtering data from series
#' When we request data from series there is the possibility of filtering data
#' on the fly using metadata information about the variables and their values
#' that define the series. To get variables for a given operation see
#' [get_metadata_variables()] and to get values for a specific variable see
#' [get_metadata_values()]. See also [get_metadata_series_varval()] to get all the values at once.
#'
#' #### Filter format
#' The format is `list(id_variable1 = id_value1, id_variable2 = id_value2)`.
#' Besides:
#' - A variable can take more than one value: `list(id_variable1 = c(id_value11, id_value12), id_variable2 = id_value2)`.
#' - A variable can take a empty character "" to get all its possible values: `list(id_variable1 = id_value1, id_variable2 = "")`.
#'
#' #### Using shortcuts
#' Additionally, shortcuts can be used to filter. They simplify the filtering
#' approach by using standardized names for variable IDs and therefore simplify
#' their use. The format is:
#' `list(shortcut_variable1 = name1, shortcut_variable2 = name2)`. Besides,
#' the *values* wrapper can also be used: `list(values = c(name1, name2)`.
#' To see a list of all available shortcuts, see [get_filter_shortcuts()] function.
#' Let’s also remark that for better performance is recommended to use numeric codes.
#' @param periodicity (int): id of the periodicity of the series. Common periodicities:
#' 1 (monthly), 3 (quarterly), 6 (bi-annual), 12 (annual). To obtain a list
#' of periodicities see [get_metadata_periodicity()].
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param page (int): page number. The retrieved result of the query is paginated. Default value is set to 1.
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' Therefore, it is recommended to set it to FALSE when there is no doubt about the validity of the input parameters, including the filter.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the series according to the operation and filter specified in the function
#'
#' @examplesIf interactive()
#' # Get time series from "IPC" operation that include the filter variables
#' # and values
#' df <- get_metadata_series_filter(operation = "IPC", periodicity = 1,
#' filter = list("115"= "29", "3" = "84", "762" = ""), validate = FALSE)
#' head(df)
#'
#' @export
get_metadata_series_filter <- function(operation = NULL, filter = NULL, periodicity = NULL, det = 0, tip = NULL, lang = "ES", page = 1, validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIE_METADATAOPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(filter = list(operation = operation, filter = filter)))
  parameters <- append(parameters, list(p = list(operation = operation, p = periodicity)))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
  parameters <- append(parameters, if(page == 0) list(page = 1) else list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the data retrieved calling the API
  data <- get_api_data(url, request)

  return(data)
}

#' Get metadata information about the variables and values of series for a given operation
#'
#' @param operation (string): code of the operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information about the variables and values that
#' define the series according to the operation specified in the function
#'
#' @examplesIf interactive()
#' # Get metadata information of time series from "IPC" operation
#' df <- get_metadata_series_varval(operation = "IPC", validate = FALSE)
#' head(df)
#'
#' @export
get_metadata_series_varval <- function(operation = NULL, lang = "ES", det = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)
  # Get the metadata information of the table
  df <- get_metadata_variable_values_operation(operation, verbose, validate, lang, det = det, request = request)

  return(df$values)
}


