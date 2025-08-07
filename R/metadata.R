#' Get all available operations
#'
#' @param operation (string): code of the operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' If no operation is specified then all the operations will be shown
#' @param lang  (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param geo (int): set to 0 for operations with national data or set to 1 for operations with data with
#' a greater level of disaggregation.
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages).
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the available operations
#'
#' @examplesIf interactive()
#' # Get all operations
#' df <- get_metadata_operations()
#' head(df)
#'
#' # Get a specific operation
#' df <- get_metadata_operations(operation = "IPC", validate = FALSE)
#' head(df)
#'
#' # Get operations with territorial disaggregation
#' df <- get_metadata_operations(geo = 1)
#' head(df)
#'
#' @export
get_metadata_operations <- function(operation = NULL, lang = "ES", geo = NULL, page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "OPERACIONES_DISPONIBLES") else list(fun = "OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation_active_null"))

  # List of parameters to call the API
  parameters <- list()
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

#' Get all available variables
#'
#' @param operation (string): Code of the operation. Provide code to get all
#' the variables for the given operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' If no operation is specified then all the variables will be shown.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages).
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the available variables
#'
#' @examplesIf interactive()
#' # Gel all variables
#' df <- get_metadata_variables()
#' head(df)
#'
#' # Get variables for a specific operation
#' df <- get_metadata_variables(operation = "IPC", validate = FALSE)
#' head(df)
#'
#' @export
get_metadata_variables <- function(operation = NULL, lang = "ES", det = 0, page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "VARIABLES") else list(fun = "VARIABLES_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation_active_null"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
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

#' Get all values for a specific variable
#'
#' @param operation (string): code of the operation. Provide code to get all
#' the values for the given operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' @param variable (int): id of a variable. To obtain a list of
#' available variables see [get_metadata_variables()].
#' @param value (int): id of a value. If an id value is specified, the children
#' of the value are requested. To obtain a list of
#' available values for a variable use `get_metadata_values(variable = id_variable)`.
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages).
#' @param classification (int): id of a classification. To obtain a list of available
#' classifications see [get_metadata_classifications()].
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#' @param hierarchy (int): depth of the hierarchy tree.
#' @param filter (list): list of variables and values. When we request the hierarchy tree
#' there is the possibility of filtering using metadata information about the variables and their values
#' that define the series.
#' The format is `list(id_variable1 = id_value1, id_variable2 = id_value2)`. Besides:
#' - A variable can take more than one value: `list(id_variable1 = c(id_value11, id_value12), id_variable2 = id_value2)`.
#' - A variable can take a empty character "" to get all its possible values: `list(id_variable1 = id_value1, id_variable2 = "")`.
#'
#' @return Data frame with information of the available values for the variable specified in the function
#'
#' @examplesIf interactive()
#' # Get the values of the variable "115"
#' df <- get_metadata_values(variable = 115)
#' head(df)
#'
#' # Get the values of a variable for a specific operation
#' df <- get_metadata_values(operation = "IPC", variable = 115, validate = FALSE)
#' head(df)
#'
#' # Get the children of a value (provinces of Galicia)
#' # Variable: Autonomous communities (id=70)
#' # Value: Galicia (id=9008)
#' df <- get_metadata_values(variable = 70, value = 9008)
#' head(df)
#'
#' @export
get_metadata_values <- function(operation = NULL, variable =  NULL, value = NULL, det = 0, lang = "ES", page = 0, classification = NULL, validate = TRUE, verbose = FALSE, hierarchy = NULL, filter = NULL){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))

  if(is.null(operation)){
    if(is.null(value)){
      definition <- append(definition, list(fun = "VALORES_VARIABLE"))
    }else{
      definition <- append(definition, list(fun = "VALORES_HIJOS"))
    }
  }else{
    definition <- append(definition, list(fun = "VALORES_VARIABLEOPERACION"))
  }

  definition <- append(definition, if(is.null(operation)) list(input = list(variable = variable, value = value)) else list(input = list(variable = variable, operation = operation)))
  definition <- append(definition, list(tag = "variable_operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(page = page))
  parameters <- append(parameters, list(clasif = list(operation = operation, clasif = classification)))

  # List of addons
  addons <- list(validate = validate, verbose = verbose, hierarchy = hierarchy)
  addons <- append(addons, if(is.null(filter)) list(filter = filter) else list(filter = list(variable = variable, filter = filter)))

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- NULL
  if(is.null(operation)){
    if(is.null(value)){
      data <- get_api_data_all_pages(url, request)
    }else{
      data <- get_api_data(url, request)
    }
  }else{
    data <- get_api_data_all_pages(url, request)
  }

  return(data)
}

#' Get all publications
#'
#' @param operation (string): code of the operation. Provide code to get all
#' the publications for the given operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' If no operation is specified then all the publications will be shown.
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages).
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information about publications
#'
#' @examplesIf interactive()
#' # Get all publications
#' df <- get_metadata_publications()
#' head(df)
#'
#' # Get publications for a specific operation
#' df <- get_metadata_publications(operation = "IPC", validate = FALSE)
#' head(df)
#'
#' @export
get_metadata_publications <- function(operation = NULL, det = 0, lang = "ES", page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "PUBLICACIONES") else list(fun = "PUBLICACIONES_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation_active_null"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
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

#' Get the dates of a publication
#'
#' @param publication (int): id of the publication. To obtain a list of
#' available publications see [get_metadata_publications()].
#' @param det (int): level of detail. Valid values: 0, 1 or 2.
#' @param tip (string): set to 'A' for friendly output (e.g. readable dates),
#' set to 'M' to include metadata or set to 'AM' for both.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages).
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the dates of the publication specified in the function
#'
#' @examplesIf interactive()
#' # Get the dates of a publication
#' df <- get_metadata_publication_dates(publication = 8, validate = FALSE)
#' head(df)
#'
#' @export
get_metadata_publication_dates <- function(publication = NULL, det = 0, tip = NULL, lang = "ES", page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "PUBLICACIONFECHA_PUBLICACION"))
  definition <- append(definition, list(input = publication))
  definition <- append(definition, list(tag = "publication"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
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

#' Get all available periodicities
#'
#' @param operation (string): Code of the operation. Provide code to get all
#' the periodicities for the given operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' If no operation is specified then all the periodicities will be shown.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the available periodicities
#'
#' @examplesIf interactive()
#' # Get all periodicities
#' df <- get_metadata_periodicity()
#' head(df)
#'
#' # Get periodicities for a specific operation
#' df <- get_metadata_periodicity(operation = "IPC", validate = FALSE)
#' head(df)
#'
#' @export
get_metadata_periodicity <- function(operation = NULL, lang = "ES", validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "PERIODICIDADES") else list(fun = "PERIODICIDAD_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation_active_null"))

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

#' Get all available filter shortcuts
#'
#' @param lang (string): language. Set to 'ES' for the Spanish version of the
#' shortcuts or set to 'EN' for the English version of the shortcuts.
#' @param validate (logical): validate input parameters.
#' @param verbose (logical): print additional information.
#'
#' @return Data frame with information of the available filter shortcuts
#'
#' @examples
#' # Shortcuts in spanish
#' df <- get_filter_shortcuts()
#' head(df)
#'
#' # Shortcuts in english
#' df <- get_filter_shortcuts(lang = "EN")
#' head(df)
#'
#' @export
get_filter_shortcuts <- function(lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))

  # List of parameters to call the API
  parameters <- list()

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  short <- names(shortcuts_filter)
  varid <- character()
  comment <- character()
  language <- character()

  for (s in short){
    varid <- append(varid, paste0(shortcuts_filter[[s]], collapse = ", "))
    comment <- append(comment, shortcuts_filter_comments[[s]]$comment)
    language <- append(language, shortcuts_filter_comments[[s]]$lang)
  }

  # Add values wrapper
  short <- append(short, "values")
  varid <- append(varid, "-")
  comment <- append(comment, "Values wrapper")
  language <- append(language, "ALL")

  # Select language
  df <- data.frame(Shortcut = short, "Variable.ID" = varid, Comment = comment, language = language)

  # Select columns
  df <- subset(df, language %in% c(lang, "ALL"), select = c("Shortcut", "Variable.ID", "Comment"))

  return(df)
}

#' Get all available classifications
#'
#' @param operation (string): Code of the operation. Provide code to get all
#' the classifications for the given operation. To obtain a list of
#' available operations see [get_metadata_operations()].
#' If no operation is specified then all the classifications will be shown.
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate input parameters. A FALSE value means fewer API calls.
#' @param verbose (logical): print additional information, including the URL to call the API service.
#'
#' @return Data frame with information of the available classifications
#'
#' @examplesIf interactive()
#' # Get all classifications
#' df <- get_metadata_classifications()
#' head(df)
#'
#' # Get classifications for a specific operation
#' df <- get_metadata_classifications(operation = "IPC", validate = FALSE)
#' head(df)
#'
#' @export
get_metadata_classifications <- function(operation = NULL, lang = "ES", validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "CLASIFICACIONES") else list(fun = "CLASIFICACIONES_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation_active_null"))

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


