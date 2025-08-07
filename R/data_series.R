#' Get data from a specific series
#'
#' @description Retrieve data from series published by INE calling the API
#'
#' @param codSeries (string): Code of the series. For further information about
#' codes click this [link](https://uvima.github.io/ineapir/articles/identify_codes.html).
#' @param nlast (int): number of periods to retrieve. By default is set to 1 period.
#' @param dateStart (string): the initial date of the requested data. The required
#' format is yyyy/mm/dd. Additionally, dateStart can be a vector of dates, where
#' each date represents the start date of individual ranges where the end date should be found
#' at the same position in the dateEnd vector. If dateStart and dateEnd are equal,
#' the specified dates are retrieved. If no end date is entered,
#' all dates will be queried, from the corresponding start date to the last available period.
#' @param dateEnd (string): the end date of the requested data. The required
#' format is yyyy/mm/dd. Additionally, dateEnd can be a vector of dates, where
#' each date represents the end date of individual ranges where the initial date should be found
#' at the same position in the dateStart vector. The length of the dateEnd vector
#' must be less than or equal to the length of the dateStart vector.
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#' @param unnest (logical): set to TRUE to obtain a single data frame of data
#'
#' @return Data frame with data of a series according to the code specified in the function
#'
#' @examplesIf interactive()
#  # Get the last five periods of time series with code "IPC251856"
#' df <- get_data_series(codSeries = "IPC251856", nlast = 5)
#' head(df)
#'
#' # Get data for an open range date
#' df <- get_data_series(codSeries = "IPC251856", dateStart = "2024/01/01")
#' head(df)
#'
#' # Get data for a single range data
#' df <- get_data_series(codSeries = "IPC251856",
#' dateStart = "2023/01/01", dateEnd = "2023/05/01")
#' head(df)
#'
#' # Get data for specific dates
#' df <- get_data_series(codSeries = "IPC251856",
#' dateStart = c("2023/01/01","2024/01/01"),
#' dateEnd = c("2023/01/01","2024/01/01"))
#' head(df)
#'
#' # Get data for multiple date ranges
#' df <- get_data_series(codSeries = "IPC251856",
#' dateStart = c("2023/01/01","2024/01/01"),
#' dateEnd = c("2023/03/01","2024/03/01"))
#' head(df)
#'
#' @export
get_data_series <- function(codSeries = NULL, nlast = 1, dateStart = NULL, dateEnd = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE, unnest = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "DATOS_SERIE"))
  definition <- append(definition, list(input = codSeries))
  definition <- append(definition, list(tag = "codSeries"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, if(is.null(dateStart) & is.null(dateEnd)) list(date = dateStart) else list(date = list(dateStart = dateStart, dateEnd = dateEnd)))
  parameters <- append(parameters, if(is.null(dateStart) & is.null(dateEnd)) list(nult = nlast) else list(nult = NULL))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose, unnest = unnest)

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

#' Get data from series for a specific operation given a filter
#'
#' @param operation (string): Code of the operation. To obtain a list of
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
#' @param nlast (int): number of periods to retrieve. By default is set to 1 period.
#' @param dateStart (string): the initial date of the requested data. The required
#' format is yyyy/mm/dd. Additionally, dateStart can be a vector of dates, where
#' each date represents the start date of individual ranges where the end date should be found
#' at the same position in the dateEnd vector. If dateStart and dateEnd are equal,
#' the specified dates are retrieved. If no end date is entered,
#' all dates will be queried, from the corresponding start date to the last available period.
#' @param dateEnd (string): the end date of the requested data. The required
#' format is yyyy/mm/dd. Additionally, dateEnd can be a vector of dates, where
#' each date represents the end date of individual ranges where the initial date should be found
#' at the same position in the dateStart vector. The length of the dateEnd vector
#' must be less than or equal to the length of the dateStart vector.
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param page (int): page number. The retrieved result of the query is paginated. Default value is set to 1.
#' @param validate (logical): validate the input parameters. A FALSE value means fewer API calls.
#' Therefore, it is recommended to set it to FALSE when there is no doubt about the validity of the input parameters, including the filter.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#' @param unnest (logical): obtain a single data frame of data
#'
#' @return Data frame with data of series according to the operation and filter specified in the function
#'
#' @examplesIf interactive()
#' # Get last five periods and filter data of time series from "IPC" operation
#' filter <- list("115"= "28", "3" = "84", "762" = "304092")
#' df <- get_data_series_filter(operation = "IPC", periodicity = 1,
#' nlast = 5, unnest = TRUE, validate = FALSE, filter = filter)
#' head(df)
#'
#' @export
get_data_series_filter <- function(operation = NULL, filter = NULL, periodicity = NULL, nlast = 1, dateStart = NULL, dateEnd = NULL, det = 0, tip = NULL, lang = "ES", page = 1, validate = TRUE, verbose = FALSE, unnest = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "DATOS_METADATAOPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(filter = list(operation = operation, filter = filter)))
  parameters <- append(parameters, list(p = list(operation = operation, p = periodicity)))
  parameters <- append(parameters, if(is.null(dateStart) & is.null(dateEnd)) list(date = dateStart) else list(date = list(dateStart = dateStart, dateEnd = dateEnd)))
  parameters <- append(parameters, if(is.null(dateStart) & is.null(dateEnd)) list(nult = nlast) else list(nult = NULL))
  #parameters <- append(parameters, list(nult = nlast))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
  parameters <- append(parameters, if(page == 0) list(page = 1) else list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose, unnest = unnest)

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
