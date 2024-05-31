# URL root to call the API
API_URL = "https://servicios.ine.es/wstempus/js"

# API version
API_version <- 3

# Number of rows per page
page_lenght = 500

# Shortcuts used in filters
shortcuts_filter <- list(nac = "349",                 # national
                         prov = "115" ,                      # provinces
                         ccaa = "70",                        # ccaa
                         mun = "19",                         # municipalities
                         isla = "20", island = "20",         # islas
                         grupo = "762", group = "762",       # cpi groups
                         subgrupo = "763", subgroup = "763", # cpi subgroups
                         clase = "764", class = "764",       # cpi class
                         subclase = "765", subclass = "765", # cpi subclass
                         rubrica = "270",                    # cpi headings
                         heading = "270",                    # cpi headings
                         grupoespecial = "269",              # cpi special groups
                         specialgroup = "269",               # cpi special groups
                         tipodato = "3", datatype = "3",     # type of data
                         sexo = "18", sex = "18",            # sex
                         edad1 = "355", age1 = "355",        # simple age
                         edadt = "356", aget = "356",        # total age
                         edadg = "360", ageg = "360",        # age groups
                         edads = "357", ages = "357",        # semi-age intervals
                         edad = c("355", "356", "360", "357"),
                         age = c("355", "356", "360", "357"),
                         nacionalidad = "141", nationality = "141",
                         generacion = "612", generation = "612",  # generation ages
                         paisnacimiento = c("431", "432"),   # country of birth
                         birthcountry = c("431", "432"),     # country of birth
                         lugarnacimiento = c("97"),          # place of birth
                         birthplace = c("97"),               # place of birth
                         efectoscorr = "544",                # correction of effects
                         effectscorr = "544"                 # correction of effects
)

shortcuts_filter_comments <- list(nac = list(comment ="National", lang = "ALL"),
                                  prov = list(comment ="Provinces", lang = "ALL") ,
                                  ccaa = list(comment ="Autonomous Communities", lang = "ALL"),
                                  mun = list(comment ="Municipalities", lang = "ALL"),
                                  isla = list(comment ="Islands", lang = "ES"),
                                  island = list(comment ="Islands", lang = "EN"),
                                  grupo = list(comment ="CPI groups", lang = "ES"),
                                  group = list(comment ="CPI groups", lang = "EN"),
                                  subgrupo = list(comment ="CPI subgroups", lang = "ES"),
                                  subgroup = list(comment ="CPI subgroups", lang = "EN"),
                                  clase = list(comment ="764", lang = "ES"),
                                  class = list(comment ="764", lang = "EN"),
                                  subclase = list(comment ="CPI class", lang = "ES"),
                                  subclass = list(comment ="CPI class", lang = "EN"),
                                  rubrica = list(comment ="CPI headings", lang = "ES"),
                                  heading = list(comment ="CPI headings", lang = "EN"),
                                  grupoespecial = list(comment ="CPI special groups", lang = "ES"),
                                  specialgroup = list(comment ="CPI special groups", lang = "EN"),
                                  tipodato = list(comment ="Type of data", lang = "ES"),
                                  datatype = list(comment ="Type of data", lang = "EN"),
                                  sexo = list(comment ="Sex", lang = "ES"),
                                  sex = list(comment ="Sex", lang = "EN"),
                                  edad1 = list(comment ="Simple age", lang = "ES"),
                                  age1 = list(comment ="Simple age", lang = "EN"),
                                  edadt = list(comment ="Age totals", lang = "ES"),
                                  aget = list(comment ="Age totals", lang = "EN"),
                                  edadg = list(comment ="Age groups", lang = "ES"),
                                  ageg = list(comment ="Age groups", lang = "EN"),
                                  edads = list(comment ="Age semi-intervals", lang = "ES"),
                                  ages = list(comment ="Age semi-intervals", lang = "EN"),
                                  edad = list(comment ="Age wrapper", lang = "ES"),
                                  age = list(comment ="Age wrapper", lang = "EN"),
                                  nacionalidad = list(comment ="Nationality", lang = "ES"),
                                  nationality = list(comment ="Nationality", lang = "EN"),
                                  generacion = list(comment ="Generation/ages", lang = "ES"),
                                  generation = list(comment ="Generation/ages", lang = "EN"),
                                  paisnacimiento = list(comment ="Country of birth", lang = "ES"),
                                  birthcountry = list(comment ="Country of birth", lang = "EN"),
                                  lugarnacimiento = list(comment ="Place of birth", lang = "ES"),
                                  birthplace = list(comment ="Place of birth", lang = "EN"),
                                  efectoscorr = list(comment ="Correction of effects", lang = "ES"),
                                  effectscorr = list(comment ="Correction of effects", lang = "EN")
)

shortcut_wrapper <- c("values")

# Function to retrieve data from the aPI
get_api_data <- function(url, request){

  result <- NULL

  # Initiate a call to the aPI
  tryCatch(
    {
      # if the url is too large we used the method POST
      if(nchar(url$complete) > 2000){

        if(request$addons$verbose){
          cat(sprintf("- API URL: %s\n", url$endpointpar))
        }

        response <- httr::VERB("POST",
                               url = url$endpoint,
                               query = url$parameters,
                               body = url$filter,
                               encode = "form",
                               httr::content_type("application/x-www-form-urlencoded"),
                               httr::user_agent("ineapir")
        )

        # we use the GET method
      }else{
        if(request$addons$verbose){
          cat(sprintf("- API URL: %s\n", url$complete))
        }

        response <- httr::VERB("GET",
                               url = url$endpoint,
                               query = url$totalpar,
                               httr::user_agent("ineapir")
        )

      }
      # Get the content of the response
      content <- httr::content(response, "text")

      if(jsonlite::validate(content)){
        result <- jsonlite::fromJSON(content , flatten = TRUE)
      }
    },
    error=function(e) {
      message('An error occurred calling the API')
      print(e)
    },
    warning=function(w) {
      message('A warning occurred calling the API')
      print(w)
    }
  )

  # Check the result retrieved for the API
  if(check_result(result)){

    # extract metadata to columns
    if((!is.null(request$addons$metanames) && request$addons$metanames) ||
       (!is.null(request$addons$metacodes) && request$addons$metacodes)){
      result <- extract_metadata(result, request)
    }

    # Unnest the Data column in one single dataframe
    if(!is.null(request$addons$unnest) && request$addons$unnest){
      result <- unnest_data(result)
    }
  }

  return(result)
}

# Function to retrieve data from the aPI when the result is paginated
get_api_data_all_pages <- function(url, request){

  result <- NULL

  # Request all pages
  if(request$parameters$page == 0){
    # Page counter
    numpage <- 1

    # Update page
    request$parameters[["page"]] <- numpage

    # Build the URL to call the API
    url <- get_url(request)

    # Call de API
    result <- get_api_data(url, request)

    # Number of rows
    numrows <- if(!is.null(nrow(result))) nrow(result) else -1

    # if the number of rows is equal to the length of a page, we query the next page
    while (numrows > 0){
      numpage <- numpage + 1

      # Update page
      request$parameters[["page"]] <- numpage

      # Update the URL to call the API
      url <- get_url(request)

      # Call the API
      resultpage <- get_api_data(url, request)

      # Number of rows
      numrows <- if(!is.null(nrow(resultpage))) nrow(resultpage) else -1

      # Accumulated result
      result <- rbind(result, resultpage)
    }
    # Request a specific page
  }else{
    result <- get_api_data(url, request)
  }

  return(result)

}


# Get the url endpoint
get_definition_path <- function(request){

  path <- ""

  # Build the definition part. We remove tag (last one) from definition
  for(x in unlist(request$definition[-length(request$definition)])){
    if(!is.null(x)){
      path <- paste0(path,"/", x)
    }
  }

  return(path)
}

# Get url parameters minus filter
get_parameters_query <- function(request){

  parameters <- list()

  for(x in names(request$parameters)){
    val <- request$parameters[[x]]

    # Discard parameters with null value
    if(x != "filter" && !is.null(val)){

      # We have to format the input date
      if(x == "date"){
        val <- build_date(val)

        # Since the list also contains the operation
      }else if(x == "p"){
        val <- val[[x]]
      }

      parameters[[x]] <- val
    }
  }

  # include version API
  parameters[["ver"]] <- API_version

  return(parameters)

}

# Get url filter
get_parameters_filter <- function(request){

  val <- request$parameters[["filter"]]

  if(!is.null(val)){
    val <- build_filter(val, request$definition, request$addons, request$check$parameters$filter)
  }

  return(val)
}

# Get url and its components
get_url <- function(request){
  # API url
  url <- httr::parse_url(API_URL)

  # Get the endpoint
  definition <- get_definition_path(request)

  # Get the parameters of the query
  parameters <- get_parameters_query(request)

  # Get the filter of the query
  parfilter <- get_parameters_filter(request)

  # Update the path with the endpoint
  url$path <- paste0(url$path, definition)

  # Build the url endpoint
  endpoint <- httr::build_url(url)

  # Update the query of the url
  url$query <- parameters

  # Build the url with parameters
  endpointpar <- httr::build_url(url)

  # Update the query of the url adding the filter
  url$query <- append(parameters, parfilter)

  # Build the complete url
  complete <- httr::build_url(url)

  result <- list(complete = complete,
                 endpoint = endpoint,
                 endpointpar = endpointpar,
                 parameters = parameters,
                 filter = parfilter,
                 totalpar = append(parameters, parfilter)
  )

  return(result)
}

# Return the dates in the format used by the API
build_date <- function(date){
  dateStart <- format.Date(date$dateStart,'%Y%m%d')
  dateEnd <- format.Date(date$dateEnd,'%Y%m%d')

  return(paste0(dateStart, ":", dateEnd))
}

# Return the cross of variables and values in the format used by the API
build_filter <- function(parameter, definition, addons, checkfilter){
  # Values to return
  val <- character()
  lval <- list()

  # id to identify a table or a operation
  id <- parameter[[1]]

  # List of variables and values
  filter <- parameter[[2]]

  # Names in the list of the parameter
  parnames <- tolower(names(parameter))

  if(addons$verbose){
    cat("- Processing filter: 0% \r")
  }

  # If validate = TRUE there exists a dataframe with values
  if(addons$validate){
    # Dataframe with the values
    dfval <- checkfilter$values

    # If there are shortcuts present in the filter
    shortcut <- checkfilter$shortcut

    # Type of object: pxtable, tempus table or series
    origin <- checkfilter$origin

    # If validate = FALSE we get the values
  }else{
    # If there are shortcuts present in the filter
    shortcut <- check_shortcut(filter, definition)

    # Dataframe with the values
    dfval <- get_filter_values(parameter, definition$lang, shortcut, verbose = FALSE, progress = addons$verbose)

    if(!is.null(dfval)){
      origin <- dfval$origin
      dfval <- dfval$values
    }
  }

  # We go through all the variables
  i <- 1
  j <- 1
  for(n in names(filter)){

    # check if in the filter there are shortcuts
    short <- is.element(tolower(n), c(names(shortcuts_filter), shortcut_wrapper))

    # It is necessary to include shortcut in the case of a px table with a code equal to a shortcut (eg sexo)
    if(shortcut && short){
      # filter with ids
      filterout <- list()

      if(origin == "tablepx"){
        # Select codes
        varid <- unique(dfval$Variable.Codigo)

        # We select only the values of variables present in the filter
        dfvalfilter <- subset(dfval, dfval$Variable.Codigo %in% varid)

      }else if(origin == "tablepxid"){
        # Select codes
        varid <- c(unique(dfval$Variable.Codigo), unique(dfval$Variable.Id))

        # We select only the values of variables present in the filter
        dfvalfilter <- subset(dfval, dfval$Variable.Codigo %in% varid | dfval$Variable.Id %in% varid)

      }else{
        if(tolower(n) %in% shortcut_wrapper){
          # Select ids
          varid <- unique(dfval$Fk_Variable)

        }else{
          # id of variables
          varid <- shortcuts_filter[[tolower(n)]]
        }

        # We select only the values of variables present in the filter
        dfvalfilter <- subset(dfval, dfval$Fk_Variable %in% varid)
      }

      # Reset the values found
      dfvalgrep <- NULL

      # Find a match between the filter inputs and the possible values
      for(f in filter[[n]]){

        ### Way one:  find a value for the largest word
        # Split the phrase
        valshort1 <- if(nchar(f) > 0 ) unlist(strsplit(as.character(f), "\\s+")) else f

        # Find the largest word
        valshort1 <- valshort1[which.max(nchar(valshort1))]

        # Find a match for the largest word and the possible values
        ind1 <- grepl(valshort1, dfvalfilter$Nombre, ignore.case = TRUE)

        # Dataframe with the matches
        dfvalgrep1 <- subset(dfvalfilter, ind1)

        ### Way two: find a value for the entire string
        # Find a match for the entire phrase and the possible values
        ind2 <- grepl(f, dfvalfilter$Nombre, ignore.case = TRUE)

        # Dataframe with the matches
        dfvalgrep2 <- subset(dfvalfilter, ind2)

        # Intersect the values from these two different ways
        if(nrow(dfvalgrep1) > 0 && nrow(dfvalgrep2) == 0){
          dfvalgreptmp <- dfvalgrep1

        }else if(nrow(dfvalgrep1) == 0 && nrow(dfvalgrep2) > 0){
          dfvalgreptmp <- dfvalgrep2

        }else if(nrow(dfvalgrep1) > 0 && nrow(dfvalgrep2) > 0){
          if(origin == "tablepx"){
            dfvalgrep2 <- subset(dfvalgrep2, select = c("Codigo", "Variable.Codigo"))
            dfvalgreptmp <- merge(dfvalgrep1, dfvalgrep2, by = c("Codigo", "Variable.Codigo"))

          }else if(origin == "tablepxid"){
            dfvalgrep2 <- subset(dfvalgrep2, select = c("Id", "Variable.Id"))
            dfvalgreptmp <- merge(dfvalgrep1, dfvalgrep2, by = c("Id", "Variable.Id"))

          }else{
            dfvalgrep2 <- subset(dfvalgrep2, select = c("Id", "Fk_Variable"))
            dfvalgreptmp <- merge(dfvalgrep1, dfvalgrep2, by = c("Id", "Fk_Variable"))
          }
        }else{
          dfvalgreptmp <- dfvalgrep1
        }

        # If there is no match result look in the id
        if(nrow(dfvalgreptmp) == 0){
          if(origin == "tablepx"){
            dfvalgreptmp <- subset(dfvalfilter, grepl(paste0("^",f,"$"), dfvalfilter$Codigo))

          }else if(origin == "tablepxid"){
            dfvalgreptmp <- subset(dfvalfilter, grepl(paste0("^",f,"$"), c(dfvalfilter$Codigo, dfvalfilter$Id)))

          }else{
            dfvalgreptmp <- subset(dfvalfilter, grepl(paste0("^",f,"$"), dfvalfilter$Id))
          }
        }

        # We add a column with the counter
        dfvalgreptmp$i <- rep(i,nrow(dfvalgreptmp))

        # Transform the filter in a the format used by the API
        if(nchar(f) > 0){

          # When grep found something
          if(nrow(dfvalgreptmp) > 0){

            # We go through all the matches
            for(r in 1:nrow(dfvalgreptmp)){
              if(origin == "tablepx"){
                # Variable code
                var <- dfvalgreptmp$Variable.Codigo[r]

                # Value code
                filterout[[var]] <- dfvalgreptmp$Codigo[r]

              }else if(origin == "tablepxid"){
                # Variable id
                var <- dfvalgreptmp$Variable.Id[r]

                # Value id
                filterout[[var]] <- dfvalgreptmp$Id[r]

              }else{
                # Variable id
                var <- dfvalgreptmp$Fk_Variable[r]

                # Value id
                filterout[[var]] <- dfvalgreptmp$Id[r]

                if(exists("dfvalgrep") && is.data.frame(get("dfvalgrep")) ){

                  # If the variable id has been used in the filter, set the same counter
                  if(is.element(var, dfvalgrep$Fk_Variable)){
                    i <- dfvalgrep[dfvalgrep$Fk_Variable == var,]$i[1]
                    dfvalgreptmp$i[r] <- i
                  }else{
                    if(nrow(dfvalgrep) > 0){
                      i <- max(dfvalgrep$i) + 1
                    }
                  }
                }
              }

              # Check the filter comes from a table or a series
              parurl <- if(is.element("idtable",parnames)) "tv" else paste0("g", i)

              # Build the filter with the format of the API
              tmp <- paste0(parurl, "=", var, ":", filterout[[var]])

              # Vector with all the values in the format of the API
              val <- append(val, tmp)

              # List with all the values
              lval <- append(lval, list(paste0(var, ":", filterout[[var]])))
              names(lval)[length(lval)] <- parurl
            }
          }
        }else{
          # Case when the value introduced is and empty character ""
          if(length(varid) == 1){
            # value set to ""
            filterout[[varid]] <- f

            # Check the filter comes from a table or a series
            parurl <- if(is.element("idtable",parnames)) "tv" else paste0("g", i)

            # Build the filter with the format of the API
            tmp <- paste0(parurl, "=", varid, ":", filterout[[varid]])

            # Vector with all the values in the format of the API
            val <- append(val, tmp)

            # List with all the values
            lval <- append(lval, list(paste0(varid, ":", filterout[[varid]])))
            names(lval)[length(lval)] <- parurl
          }
        }

        if (exists("dfvalgrep") && is.data.frame(get("dfvalgrep"))){
          dfvalgrep <- rbind(dfvalgrep,dfvalgreptmp)
        }else{
          dfvalgrep <- dfvalgreptmp
        }

        if(addons$verbose){
          cat(sprintf("- Processing filter: %s%%        \r", round(50 + j/sum(lengths(filter))*50,0)))
        }

        i <- i + 1
        j <- j + 1
      }
      # When there are no shortcuts in the filter
    }else{
      # Check the filter comes from a table or a series
      parurl <- if(is.element("idtable",parnames)) "tv" else paste0("g", i)

      # Build the filter with the format of the API
      tmp <- paste0(parurl, "=", n, ":", filter[[n]])

      # Vector with all the values in the format of the API
      val <- append(val, tmp)

      for(f in filter[[n]]){
        # List with all the values
        lval <- append(lval, list(paste0(n, ":", f)))
        names(lval)[length(lval)] <- parurl

        if(addons$verbose){
          cat(sprintf("- Processing filter: %s%%        \r", round(50 + j/sum(lengths(filter))*50,0)))
        }

        j <- j + 1
      }

      i <- i + 1
    }
  }

  if(addons$verbose){
    cat("- Processing filter: 100%         \n")
  }

  return(lval)
}

# Get the all values used in a table or operation
get_filter_values <- function(parameter, lang, shortcut, verbose, progress = TRUE){

  # id to identify a table or a operation
  id <- parameter[[1]]

  # List of variables and values
  filter <- parameter[[2]]

  # Names in the list of the parameter
  parnames <- tolower(names(parameter))

  # Dataframe to return the values
  dfval <- NULL

  # The filter includes shortcuts in the names of variables and values
  if(shortcut){
    # The filter comes from a table
    if(is.element("idtable",parnames)){

      # Get the metadata information of the table
      dfval <- get_metadata_variable_values_table(idTable = id, verbose = verbose, validate = FALSE, lang = lang, progress = progress)

      # The filter comes from a series
    }else{
      # We obtain the variables and values from the operation of the series
      dfval <- get_metadata_variable_values_operation(operation = id, verbose = verbose, validate = FALSE, lang, progress)
    }
  }

  return(dfval)
}

# Check the request
check_request <- function(request){

  # Check addons
  cadd <- check_addons(request$parameters, request$addons, request$definition)

  # Check definition
  cdef <- check_definition(request$definition, request$addons)

  # Check parameters
  cpar <- check_parameters(request$parameters, request$addons, request$definition)

  # Check results to return
  check <- list()
  check <- append(check, list(definition = cdef))
  check <- append(check, list(addons = cadd))
  check <- append(check, list(parameters = cpar))

  request <- append(request, list(check = check))

  return(request)
}

# Check the definition of the request
check_definition <- function(definition, addons){
  result <- list()

  # Validate or not the definition
  check <- addons$validate

  if(check){
    for(x in names(definition)){
      val <- definition[[x]]

      if(!is.null(val)){
        r <- switch (x,
                     "lang" = check_lang(val, addons$verbose),
                     "input" = check_input(definition$tag, val, addons$verbose)
        )
        # Check results to return
        result <- append(result, r)
        names(result)[length(result)] <- x
      }
    }
  }

  return(result)
}

# Check the parameters of the request
check_parameters <- function(parameters, addons, definition){
  result <- list()

  # Validate or not the parameters
  check <- addons$validate

  if(check){
    for(x in names(parameters)){
      val <- parameters[[x]]

      if(!is.null(val)){
        r <- switch (x,
                     "date" = check_dates(val, addons$verbose),
                     "p" = check_periodicity(val[[1]], val[[2]], addons$verbose),
                     "nult" = check_nlast(val, addons$verbose),
                     "det" = check_det(val, addons$verbose),
                     "tip" = check_tip(val, addons$verbose),
                     "geo" = check_geo(val, addons$verbose),
                     "page" = check_page(val, addons$verbose),
                     "filter" = check_filter(val, addons$verbose, definition)
        )
        # Check results to return
        result <- append(result, r)
        names(result)[length(result)] <- x
      }
    }
  }

  return(result)
}

# Check the addons of the request
check_addons <- function(parameters, addons, definition){
  result <- list()

  for(x in names(addons)){
    val <- addons[[x]]

    if(!is.null(val)){
      r <- switch (x,
                   "validate" = check_islogical(x, val),
                   "verbose" = check_islogical(x, val),
                   "unnest" = check_islogical(x, val),
                   "metanames"= check_extractmetadata(x, val, parameters$tip),
                   "metacodes"= check_extractmetadata(x, val, parameters$tip)
      )
      # Check results to return
      result <- append(result, r)
      names(result)[length(result)] <- x
    }
  }

  return(result)
}

#Check the result retrieved for the API
check_result <- function(result){
  check <- FALSE

  if(!is.null(result)){
    if(!check_result_status(result)){
      if(is.data.frame(result) && nrow(result) > 0){
        check <- TRUE
      }
      if(is.list(result) && !is.data.frame(result) && length(result) > 0){
        check <- TRUE
      }
    }
  }

  return(check)
}

#Check the result retrieved for the API
check_result_status <- function(result){
  check <- FALSE

  if(is.element("status", names(result))){
    check <- TRUE
    cat(sprintf("- %s\n", result$status))
  }

  return(check)
}

# check if lang argument in the definition is valid
check_lang <- function(lang, verbose){
  result <- TRUE

  if(!is.character(lang)){
    result <- FALSE
    stop("lang must be a string equal to 'ES' for Spanish or equal to 'EN' for English")
  }else{
    if(lang != "ES" && lang != "EN"){
      result <- FALSE
      stop("lang must be a string equal to 'ES' for Spanish or equal to 'EN' for English")
    }
  }

  if(verbose){
    cat(sprintf("- Check lang: OK\n"))
  }

  return(result)
}

# Check the input part of the definition
check_input <- function(tag, input, verbose){
  result <- list()

  r <- switch(
    tag,
    "operation" = check_operation(input, verbose = verbose),
    "operation_active_null" = check_operation(input, active_null = TRUE, verbose = verbose),
    "codSeries" = check_isnull(tag, input, verbose),
    "variable_operation" = check_variables_operation(input, verbose),
    "publication" = check_publication(input, verbose),
    "idTable" = check_isnull(tag, input, verbose),
    "idTable_idGroup" = check_idtable_idgroup(input, verbose)
  )
  # Check results to return
  result <- append(result, r)
  names(result)[length(result)] <- tag

  return(result)
}

# Check operation argument in API call
check_operation <- function(operation, active_null = FALSE, verbose){
  result <- TRUE

  if(!is.null(operation)){
    # Get all operations
    opes <- get_metadata_operations(validate = FALSE, verbose = verbose, page = 0)

    # Logical controls
    id <- FALSE
    ioe <- FALSE
    cod <- FALSE

    # Check id
    tmp <- opes$Id[trimws(opes$Id) != ""]

    if(!is.element(operation,tmp)){
      id <- TRUE
    }

    # Check cod_IOE
    tmp <- paste0("IOE", opes$Cod_IOE[trimws(opes$Cod_IOE) != ""])

    if(!is.element(operation,tmp)){
      ioe <- TRUE
    }

    # Check code
    tmp <- opes$Codigo[trimws(opes$Codigo) != ""]

    if(!is.element(operation,tmp)){
      cod <- TRUE
    }

    result <- !(id & ioe & cod)

    if(!result){
      stop("The operation not exists")
    }
  }else{
    if(!active_null){
      result <- FALSE
      stop("The operation must be specified")
    }
  }

  if(verbose){
    cat(sprintf("- Check operation: OK\n"))
  }

  return(result)
}

# Check variables
check_variables_operation <- function(input, verbose){
  result <- TRUE

  # Variable id
  variable <- input$variable

  # Operation id
  operation <- input$operation

  if(!is.null(operation)){
    # First we check if the operation is valid
    check_operation(operation, verbose = verbose)

    # Second we check if the variable is valid for the operation
    result <- check_variablesoperation(operation, variable, verbose)

  }else{
    # Check if the variable is valid
    result <- check_variable(variable, verbose)
  }

  return(result)
}

# Check if a variable is valid for an operation
check_variablesoperation <- function(operation, variable, verbose){
  result <- TRUE

  if(!is.null(variable)){
    vars <- get_metadata_variables(operation = operation, validate = FALSE, verbose = verbose, page = 0)

    if(!is.element(variable, vars$Id)){
      result <- FALSE
      stop(sprintf("%s is not a valid variable for operation %s. Valid ids: %s", variable, operation, paste0(vars$Id, collapse = ", ")))
    }
  }else{
    result <- FALSE
    stop("variable argument must be specified")
  }

  if(verbose){
    cat(sprintf("- Check variable: OK\n"))
  }

  return(result)
}

# Check if the variable is valid
check_variable <- function(variable, verbose){
  result <- TRUE

  if(!is.null(variable)){
    vars <- get_metadata_variables(validate = FALSE, verbose = verbose, page = 0)

    if(!is.element(variable, vars$Id)){
      result <- FALSE
      stop(sprintf("%s variable not exists", variable))
    }
  }else{
    result <- FALSE
    stop("variable argument must be specified")
  }

  if(verbose){
    cat(sprintf("- Check variable: OK\n"))
  }

  return(result)
}

# check if a publication is valid
check_publication <- function(publication, verbose){
  result <- TRUE

  if(!is.null(publication)){
    # Get all the publications
    pubs <- get_metadata_publications(validate = FALSE, verbose = verbose, page = 0)

    if(!is.element(publication, pubs$Id)){
      result <- FALSE
      stop(sprintf("%s publication not exists", publication))
    }
  }else{
    result <- FALSE
    stop("publication argument must be specified")
  }

  if(verbose){
    cat(sprintf("- Check publication: OK\n"))
  }
  return(result)
}

# Check if the argument is NULL
check_isnull <- function(name, id, verbose){
  result <- TRUE

  if(is.null(id)){
    result <- FALSE
    stop(sprintf("%s argument must be specified", name))
  }

  if(verbose){
    cat(sprintf("- Check %s: OK\n", name))
  }

  return(result)
}

# Check if both, table and group, are NULL
check_idtable_idgroup <- function(input, verbose){
  result <- TRUE

  idTable <- input$idTable
  idGroup <- input$idGroup

  nameid <- names(input)

  check_isnull(nameid[1], idTable, verbose)
  check_isnull(nameid[2], idGroup, verbose = FALSE)

  if(!is.null(idTable) && !is.null(idGroup)){
    # Get all the groups of the table
    groups <- get_metadata_table_groups(idTable = idTable, validate = FALSE, verbose = verbose)

    if(!is.element(idGroup, groups$Id)){
      result <- FALSE
      stop(sprintf("%s is not a valid group for table %s. Valid ids: %s", idGroup, idTable, paste0(groups$Id, collapse = ", ")))
    }
  }

  if(verbose){
    cat(sprintf("- Check idGroup: OK\n"))
  }

  return(result)
}

# Check date argument in API CALL
check_dates <- function(date, verbose){
  result <- TRUE

  dateStart <- date$dateStart
  dateEnd <- date$dateEnd

  namesdate = names(date)

  check_date_format(namesdate[1], dateStart)
  check_date_format(namesdate[2], dateEnd)

  if(!is.null(dateEnd)){
    if(!is.null(dateStart)){
      if(dateStart > dateEnd){
        result <- FALSE
        stop("dateStart must be previous to dateEnd.")
      }
    }else{
      result <- FALSE
      stop("dateStart must be specified.")
    }
  }

  if(verbose){
    cat(sprintf("- Check date: OK\n"))
  }

  return(result)
}

# Check the input format of the date
check_date_format <- function(name, date){
  # Remove white spaces
  date <- if(!is.null(date)) gsub("\\s+", "", date) else date

  # Input format must be yyyy/mm/dd
  format <- if(!is.null(date)) grepl("[0-9]{4}/[0-9]{2}/[0-9]{2}", date) else FALSE

  if(format){
    y <- substr(date, 1, 4)
    m <- substr(date, 6, 7)
    d <- substr(date, 9, 10)

    if(m > 12){
      stop(sprintf("%s month can not be greater than 12", name))
    }

    if(d > 31){
      stop(sprintf("%s day can not be greater than 31", name))
    }
  }else{
    if(!is.null(date)){
      stop(sprintf("%s format is not correct. Date format must be as follow: yyyy/mm/dd", name))
    }
  }
}

# check if the periodicity argument is valid
check_periodicity <- function(operation, p, verbose){

  result <- TRUE

  if(!is.null(p)){
    # Get periodicities of an operation
    periodicity <- get_metadata_periodicity(operation = operation, validate = FALSE, verbose = verbose)

    if(!is.element(p, periodicity$Id)){
      result <- FALSE
      stop(sprintf("%s is not a valid periodicity for operation %s. Valid ids: %s", p, operation, paste0(periodicity$Id, collapse = ", ")))
    }
  }else{
    result <- FALSE
    stop("periodicity must be specified")
  }

  if(verbose){
    cat(sprintf("- Check periodicity: OK\n"))
  }
  return(result)
}

# Check if the nlast argument is valid
check_nlast <- function(nlast, verbose){
  result <- TRUE

  if(!is.numeric(nlast)){
    result <- FALSE
    stop("nlast must be a number greater or equal to 1")

  }else{
    if(nlast < 1){
      result <- FALSE
      stop("nlast must be a number greater or equal to 1")
    }
  }

  if(verbose){
    cat(sprintf("- Check nlast: OK\n"))
  }

  return(result)
}

# Check if the det argument is valid
check_det <- function(det, verbose){
  result <- TRUE

  if(!is.numeric(det)){
    result <- FALSE
    stop("det must be a number between 0 and 2")
  }else{
    if(det < 0 || det > 2){
      result <- FALSE
      stop("det value must be between 0 and 2")
    }
  }

  if(verbose){
    cat(sprintf("- Check det: OK\n"))
  }

  return(result)
}

# Check if the tip argument is valid
check_tip <- function(tip, verbose){
  result <- TRUE

  if(!is.null(tip)){
    tip <- toupper(tip)

    if(tip != "A" && tip != "M" && tip != "AM" && tip != "MA"){
      result <- FALSE
      stop("tip must be equal to 'A', 'M' or 'AM'")
    }
  }

  if(verbose){
    cat(sprintf("- Check tip: OK\n"))
  }

  return(result)
}

# Check if the geo argument is valid
check_geo <- function(geo, verbose){
  result <- TRUE

  if(!is.null(geo)){
    if(!is.numeric(geo)){
      result <- FALSE
      stop("geo must be a number equal to 0 or 1")
    }else{
      if(geo < 0 || geo > 1){
        result <- FALSE
        stop("geo must be a number equal to 0 or 1")
      }
    }
  }

  if(verbose){
    cat(sprintf("- Check geo: OK\n"))
  }
  return(result)
}

# check if the page argument is valid
check_page <- function(n, verbose){
  result <- TRUE

  if(!is.numeric(n)){
    result <- FALSE
    stop("page must be a number greater or equal to 0")
  }else{
    if(n < 0){
      result <- FALSE
      stop("page must be a number greater or equal to 0")
    }
  }

  if(verbose){
    cat(sprintf("- Check page: OK\n"))
  }
  return(result)
}

# Check if the filter argument is valid
check_filter <- function(parameter, verbose, definition){
  result <- TRUE

  # If there are shortcuts in the filter
  shortcut <- FALSE

  # id to identify a table or a operation
  id <- parameter[[1]]

  # List of variables and values
  filter <- parameter[[2]]

  # Names in the list of the parameter
  parnames <- tolower(names(parameter))

  # Get the values from metadata of tables or operations
  df <- get_filter_values(parameter, definition$lang, shortcut = TRUE, verbose = verbose, progress = FALSE)

  # Make sure the response is valid or null
  if(!check_result_status(df$values)){

    # The filter comes from a px table
    if(df$origin == "tablepx"){
      check <- check_table_px_filter(id, filter, verbose, df$values)

      result <- check$result
      shortcut <- check$shortcut

      # The filter comes from a px table with ids
    }else if(df$origin == "tablepxid"){
      check <- check_table_px_id_filter(id, filter, verbose, df$values)

      result <- check$result
      shortcut <- check$shortcut

      # The filter comes from a tempus table
    }else if(df$origin == "tablet3"){
      check <- check_table_tempus_filter(parameter, verbose, df$values)

      result <- check$result
      shortcut <- check$shortcut
    }

    # The filter comes from a series
    else if(df$origin == "series") {
      check <- check_series_filter(parameter, verbose, df$values)

      result <- check$result
      shortcut <- check$shortcut
    }
  }

  # If there are shortcuts in the filter
  df$shortcut <- shortcut

  return(list(df))
}

# Confirm if the metadata of the table contains information about the values id
exists_values_id <- function(metadata){
  result <- FALSE

  # Column names of the metadata of the table
  metacols <- tolower(unique(unlist(lapply(metadata, names))))

  # If there is a id column then
  if(is.element("id", metacols)){
    result <- TRUE
  }

  return(result)
}

# Confirm if the metadata of the table contains information about the values id
exists_variables_id <- function(metadata){
  result <- FALSE
  colname <- ""

  # Column names of the metadata of the table
  metacols <- unique(unlist(lapply(metadata, names)))

  # If there is a id column
  if(is.element("variable.id", tolower(metacols))){
    result <- TRUE
    colname <- metacols[grep("variable.id", metacols, ignore.case = TRUE)]
  }

  if(is.element("fk_variable", tolower(metacols))){
    result <- TRUE
    colname <- metacols[grep("fk_variable", metacols, ignore.case = TRUE)]
  }

  return(list(result = result, name = colname))
}

# Check if the filter argument is valid for a px table
check_table_px_filter <- function(idTable, pxfilter, verbose, df){
  result <- TRUE

  # If there are shortcuts in the filter
  shortcut <- FALSE

  # The filter must be a list
  if(is.list(pxfilter)){

    # Variables of the filter
    var <- names(pxfilter)

    # Go through all the variables
    for(v in var){
      # If the variable in the filter is not in the metadata is not valid
      if(!is.element(v, c(df$Variable.Codigo, shortcut_wrapper))){
        result <- FALSE
        msg <- sprintf("%s is not a valid variable for %s idTable. Valid variable codes: %s",v,idTable, paste0(unique(df$Variable.Codigo), collapse = ", "))
        msg <- if(is.element(v, names(shortcuts_filter))) paste0(msg,"\nThe only shortcut valid for this table is the wrapper 'values'") else msg
        stop(msg)
      }

      # Has been used a shortcut name for the variable or not
      short <- is.element(tolower(v), shortcut_wrapper)

      # Identify a filter with shortcuts
      shortcut <- shortcut | short

      # subset of the metadata for an specific variable
      metavar <- if(v %in% shortcut_wrapper) df else df[df$Variable.Codigo == v,]

      # Go through all the values in the filter for the specific variable
      for(val in pxfilter[[v]]){

        # Split the value
        valshort <- if(nchar(val) > 0 ) unlist(strsplit(as.character(val), "\\s+")) else val

        validnames <- TRUE
        for(vs in valshort){
          validnames <- validnames & sum(grepl(vs, metavar$Nombre, ignore.case = TRUE)) > 0
        }

        # If the value in the filter is not in the metadata is not valid
        if(val != "" && !(is.element(val, metavar$Codigo) || validnames )){
          result <- FALSE
          stop(sprintf("%s is not a valid value for variable %s", val, v))
        }
      }
    }
  }else{
    result <- FALSE
    stop("filter must be a list")
  }

  if(verbose){
    cat(sprintf("- Check filter: OK\n"))
  }

  return(list(result = result, shortcut = shortcut))
}

# Check if the filter argument is valid for a px table
check_table_px_id_filter <- function(idTable, pxfilter, verbose, df){
  result <- TRUE

  # If there are shortcuts in the filter
  shortcut <- FALSE

  # The filter must be a list
  if(is.list(pxfilter)){

    pxfilter <- check_alias_filter(pxfilter)

    # Variables of the filter
    var <- names(pxfilter)

    # Go through all the variables
    for(v in var){
      # If the variable in the filter is not in the metadata is not valid
      if(!is.element(v, c(df$Variable.Codigo, df$Variable.Id, shortcut_wrapper))){
        result <- FALSE
        msg <- sprintf("%s is not a valid variable for %s idTable. Valid variable codes: %s. Valid variable ids: %s",
                       v,
                       idTable,
                       paste0(unique(df$Variable.Codigo[nchar(df$Variable.Codigo) > 0]), collapse = ", "),
                       paste0(unique(df$Variable.Id), collapse = ", "))
        msg <- if(is.element(v, names(shortcuts_filter))) paste0(msg,"\nThe only shortcut valid for this table is the wrapper 'values'") else msg
        stop(msg)
      }

      # Has been used a shortcut name for the variable or not
      short <- is.element(tolower(v), shortcut_wrapper)

      # Identify a filter with shortcuts
      shortcut <- shortcut | short

      # subset of the metadata for an specific variable
      metavar <- if(v %in% shortcut_wrapper) df else df[df$Variable.Codigo == v | df$Variable.Id == v,]

      # Go through all the values in the filter for the specific variable
      for(val in pxfilter[[v]]){
        # Split the value
        valshort <- if(nchar(val) > 0 ) unlist(strsplit(as.character(val), "\\s+")) else val

        validnames <- TRUE
        for(vs in valshort){
          validnames <- validnames & sum(grepl(vs, metavar$Nombre, ignore.case = TRUE)) > 0
        }

        # If the value in the filter is not in the metadata is not valid
        if(val != "" && !(is.element(val, c(metavar$Codigo, metavar$Id)) || validnames )){
          result <- FALSE
          stop(sprintf("%s is not a valid value for variable %s", val, v))
        }
      }
    }
  }else{
    result <- FALSE
    stop("filter must be a list")
  }

  if(verbose){
    cat(sprintf("- Check filter: OK\n"))
  }

  return(list(result = result, shortcut = shortcut))
}

# Check if the filter argument is valid for a tempus table
check_table_tempus_filter <- function(parameter, verbose, df){
  result <- TRUE

  # If there are shortcuts in the filter
  shortcut <- FALSE

  # id to identify a table or a operation
  id <- parameter[[1]]

  # List of variables and values
  filter <- parameter[[2]]

  # Names in the list of the parameter
  parnames <- tolower(names(parameter))

  # The filter must be a list
  if(is.list(filter)){
    check <- check_tempus_filter(id, filter, parnames, df)
    shortcut <- check$shortcut

  }else{
    result <- FALSE
    stop("filter must be a list")
  }

  if(verbose){
    cat(sprintf("- Check filter: OK\n"))
  }

  return(list(result = result, shortcut = shortcut))
}

# Check if the filter argument is valid for a series
check_series_filter <- function(parameter, verbose, df){
  result <- TRUE

  # If there are shortcuts in the filter
  shortcut <- FALSE

  # id to identify a table or a operation
  id <- parameter[[1]]

  # List of variables and values
  filter <- parameter[[2]]

  # Names in the list of the parameter
  parnames <- tolower(names(parameter))

  # The filter must be a list
  if(is.list(filter)){

    # Variables of the filter
    var <- names(filter)

    # Values of the filter
    val <- unlist(filter, use.names = FALSE)

    # The list must contain at least two values in the filter
    if(length(var) > 1 || (length(var) < 2 && length(val) > 1)){
      check <- check_tempus_filter(id, filter, parnames, df)
      shortcut <- check$shortcut

    }else{
      result <- FALSE
      stop("The list must contain at least two values in the filter")
    }
  }else{
    result <- FALSE
    stop("filter must be a list")
  }


  if(verbose){
    cat(sprintf("- Check filter: OK\n"))
  }

  return(list(result = result, shortcut = shortcut))
}

# Check if the filter argument is valid
check_tempus_filter <- function(id, filter, parnames, df){
  result <- TRUE

  # If there are shortcuts in the filter
  shortcut <- FALSE

  # Variables of the filter
  var <- names(filter)

  for(v in var){
    # Has been used a shortcut name for the variable or not
    short <- is.element(tolower(v), c(names(shortcuts_filter), shortcut_wrapper))

    # Identify a filter with shortcuts
    shortcut <- shortcut | short

    if(short){
      # The values wrapper is present
      if(tolower(v) %in% shortcut_wrapper){
        variable <- unique(df$Fk_Variable)

        # A shortcut is present
      }else{
        variable <- shortcuts_filter[[tolower(v)]]
      }
    }else{
      variable <- v
    }

    # The variable id is in the metadata information
    validvar <- intersect(variable, df$Fk_Variable)

    if(!(is.element(v, df$Fk_Variable) || length(validvar) > 0 )){
      result <- FALSE
      stop(sprintf("%s is not a valid variable for %s %s",v, parnames[1],id))
    }

    # If the shortcut name includes more than one variable
    # obtain the metadata information for all the variables
    metavar <- subset(df, df$Fk_Variable %in% validvar)

    # Go through all the values of an specific variable
    for(val in filter[[v]]){
      # Multiple values
      for(f in val){
        # Split the value
        valshort <- if(nchar(f) > 0 ) unlist(strsplit(as.character(f), "\\s+")) else f

        validnames <- TRUE
        # Check each part of the value
        for(vs in valshort){
          validnames <- validnames & sum(grepl(vs, metavar$Nombre, ignore.case = TRUE)) > 0
        }

        # The id or the shortcut name of the value must exist in the metadata information
        if(f != "" && !(is.element(f, metavar$Id) || validnames)){
          result <- FALSE

          if(is.element("idtable",parnames)){
            stop(sprintf("%s is not a valid value for variable %s or is not present in the values of the groups of the table", f, v))
          }else{
            stop(sprintf("%s is not a valid value for variable %s", f, v))
          }
        }
      }
    }
  }

  return(list(result = result, shortcut = shortcut))
}

# Check if an argument is logical
check_islogical <- function(name, par){
  result <- TRUE

  if(!is.logical(par)){
    result <- FALSE
    stop(sprintf("%s must be logical", name))
  }

  return(result)
}

# Check if the argument shortcut is set correctly
check_shortcut <- function(filter, definition){

  result <- FALSE

  if(!is.null(filter)){

    # Tables
    if(grepl("IdTable", definition$tag, ignore.case = TRUE)){

      # check the type of the table
      checktable <- check_type_table(idTable = definition$input)

      # PX table
      if(checktable$ispxtable){
        if(sum(is.element(tolower(names(filter)), shortcut_wrapper)) > 0){
          result <- TRUE
        }

        # Tempus table
      }else{
        # There are shortcuts in the filter
        if(sum(is.element(tolower(names(filter)), c(names(shortcuts_filter), shortcut_wrapper))) > 0 ){
          result <- TRUE
        }
      }

      # Tempus series
    }else{
      # There are shortcuts in the filter
      if(sum(is.element(tolower(names(filter)), c(names(shortcuts_filter), shortcut_wrapper))) > 0 ){
        result <- TRUE
      }
    }
  }

  return(result)
}


# Check if the extractmetadata argument is valid
check_extractmetadata <- function(name, val, tip){

  result <-  check_islogical(name, val)

  if(!is.null(tip)){
    tip <- toupper(tip)

    if(val && tip != "M" && tip != "AM" && tip != "MA"){
      result <- FALSE
      stop(sprintf("when %s is set TRUE, tip must be equal to 'M' or 'AM'", name))
    }
  }else{
    if(val){
      result <- FALSE
      stop(sprintf("when %s is set TRUE, tip must be equal to 'M' or 'AM'", name))
    }
  }

  return(result)
}

# Obtain an unique dataframe from a list of dataframes
unnest_data <- function(datain){

  # We have a dataframe
  if(is.data.frame(datain)){
    # Discard data and notas columns
    sel <- tolower(names(datain)) != "data" & tolower(names(datain)) != "notas"

    # Dataframe header without the data and notas columns
    dataout <- datain[c(),sel]

    # Data Dataframes of the list not empty
    datasel <- lengths(datain$Data) > 0

    # Only dataframes with Data
    datacol <- datain$Data[datasel]

    # Dataframes without Data
    nodata <- datain[!datasel,sel]

    # Go through all the dataframes with data
    if(length(datacol) > 0){
      # Adding Data column to the header
      dataout <- cbind(dataout,datacol[[1]][c(),])

      # Repeat each row by the number of data values
      tmp <- datain[rep(seq_len(nrow(datain)), times = sapply(datain$Data, nrow)), sel]

      # Unique dataframe of data
      data <- do.call(rbind, datain$Data)

      # Adding data
      dataout <- cbind(tmp,data)

      # In case we have only one column
      if(sum(sel) == 1){
        names(dataout)[1] <- names(datain)[1]
      }
    }

    # In case of dataframes without Data
    if(!is.null(nrow(nodata)) && nrow(nodata) > 0){

      if(length(datacol) > 0){
        # index of the dataframe with more values of data
        ind <- which.max(lapply(datacol, nrow))

        # Dataframe with values
        data <- datacol[[ind]]

        # Change value for NA
        data$Valor <- NA

        # Repeat each row by the number of data values
        tmp <- nodata[rep(seq_len(nrow(nodata)), each = nrow(data)),]

        # Repeat each row by the number of nodata rows
        nodata <- data[rep(seq_len(nrow(data)), times = nrow(nodata)),]

        # Adding columns
        nodata <- cbind(tmp, nodata)

        # Adding rows
        dataout <- rbind(dataout, nodata)

      }else{
        dataout <- datain
      }
    }
  }

  # We have a list (series case)
  if(is.list(datain) && !is.data.frame(datain)){
    # Selection of single columns
    sel <- lengths(datain) == 1

    # Selection of metadata
    selmeta <- lengths(datain) > 1 & tolower(names(datain)) != "data" & tolower(names(datain)) != "notas"

    # Dataframe without metadata, data and notas columns
    tmp <- as.data.frame(datain[sel])

    # Adding metadata
    if(sum(selmeta) > 0){
      for(n in names(selmeta)){
        if(selmeta[[n]]){
          tmp[[n]] <- datain[n]
        }
      }
    }

    # Repeat each row by the number of data values
    tmp <- tmp[rep(seq_len(nrow(tmp)), each = max(lengths(datain$Data))),]

    # Obtain data
    data <- datain$Data

    # Adding data
    dataout <- cbind(tmp,data)
  }

  return(dataout)
}


# Check if the table is a px file or not
check_type_table <- function(idTable, verbose = FALSE, validate = FALSE, lang = "ES"){

  # Get the groups of the table
  groups <- get_metadata_table_groups(idTable = idTable, verbose = verbose, validate = validate, lang = lang)

  # If the result of the query is null is a px table
  result <- if(is.null(groups)) TRUE else FALSE
  origin <- if(result) "tablepx" else "tablet3"

  return(list(groups = groups, ispxtable = result, origin = origin))
}

check_alias_filter <- function(f){

  val <- f

  # Get variables
  n <- names(f)

  # Check if there are variables with aliases
  login <- grepl("~", n)

  # Check if there are values with aliases
  logif <- grepl("~", f)

  if(sum(login) != sum(logif)){
    stop("Filter aliases missing")

  }else{
    if(sum(login) > 0){
      # Get variables without aliases
      var <- unlist(lapply(n, function(x) gsub("~id|~cod","",tolower(x))))

      # Check if the aliases are valid
      if(length(grep("~", var)) > 0){
        stop("The alias in the variables of the filter is not valid. Valid aliases are 'id' and 'cod'")
      }

      # Get values without aliases
      val <- lapply(f, function(x) gsub("~id|~cod","",tolower(x)))

      # Check if the aliases are valid
      if(length(grep("~", unlist(val))) > 0){
        stop("The alias in the values of the filter is not valid. Valid aliases are 'id' and 'cod'")
      }

      # Get aliases of  variables
      varalias <- rep(unlist(lapply(strsplit(names(f), "~"),
                                    function(x) {if(length(x) > 1) x[2] else ""})),
                      times = lengths(f))

      # Get aliases of values
      valalias <- unlist(lapply(strsplit(unlist(f, use.names = FALSE), "~"),
                                function(x) {if(length(x) > 1) x[2] else ""}))

      if(sum(varalias == valalias) != length(valalias)){
        stop("The alias of a variable and its values must be the same. Valid aliases are 'id' and 'cod'")
      }

      names(val) <- var
    }
  }

  return(val)
}

# Extract metadata information from tables into columns
extract_metadata <- function(datain, request){
  # Obtain metadata
  metadata <- datain$MetaData

  dataout <- datain

  # Metadata columns to extract
  metacols <- character()
  metacolsnames <- character()

  if(request$addons$metanames){
    metacols <- append(metacols, "Nombre")
    metacolsnames <- append(metacolsnames, "")
  }

  if(request$addons$metacodes){
    metacols <- append(metacols, "Codigo")
    metacolsnames <- append(metacolsnames, ".Codigo")
  }

  # Tables
  if(grepl("IdTable",request$definition$tag, ignore.case = TRUE)){

    # check the type of the table
    checktable <- check_type_table(idTable = request$definition$input, lang = request$definition$lang)

    # Case one: tpx or px table
    if(checktable$ispxtable){
      # Number of variables in metadata information
      nummeta <- min(unique(do.call(rbind,lapply(metadata,nrow))))

      # Check if exits and id fo variables
      existsvarid <- exists_variables_id(metadata)

      # Column to extract metadata
      varmeta <- if(exists_values_id(metadata) && existsvarid$result) existsvarid$name else "Variable.Codigo"

      if(exists_values_id(metadata)){
        metacols <- append(metacols, "Id")
        metacolsnames <- append(metacolsnames, ".Id")
      }

      # Obtain variable codes for each row in metadata information
      varcode <- list()

      for(i in 1:nummeta){
        varcode <- append(varcode,
                          as.data.frame(unique(do.call(rbind,
                                                       lapply(metadata, '[',c(i),c(varmeta))))))
      }

      # Loop through all variables
      for(var in varcode){

        # Select a variable code and build a Unique dataframe of variable names
        dfcodes <- do.call(rbind,
                           lapply(metadata,
                                  function(x) subset(x,
                                                     x[[varmeta]] %in% var,
                                                     select = metacols)))

        # Rename column with variable code
        newname <- paste0(gsub("\\s+",".", var), collapse = "_")
        names(dfcodes) <- paste0(newname, metacolsnames)

        # Adding column to dataframe
        if(nrow(dfcodes) == nrow(dataout)){
          dataout <- cbind(dataout, dfcodes)
        }
      }
      # Case two: tempus table
    }else{

      if(request$addons$metacodes){
        metacols <- append(metacols, "Id")
        metacolsnames <- append(metacolsnames, ".Id")
      }

      # Loop through all groups
      for (g in checktable$groups$Id){
        # Get th values of the group
        values <- get_metadata_table_values(idTable = request$definition$input, idGroup = g, validate = FALSE, lang = request$definition$lang)

        # Select a variable id and build a Unique dataframe of variable names
        dfcodes <- do.call(rbind,
                           lapply(metadata,
                                  function(x) subset(x,
                                                     x$Variable.Id %in% unique(values$Fk_Variable),
                                                     select = metacols)))
        # New name of the column
        newname <- unlist(subset(checktable$groups, checktable$groups$Id == g, select = c("Nombre")))
        newname <- gsub("\\s+",".", newname)

        # Rename column
        names(dfcodes) <- paste0(newname, metacolsnames)

        # Adding column to dataframe
        dataout <- cbind(dataout, dfcodes)
      }
    }
    # Series
  }else{

    # Number of variables in metadata information
    nummeta <- min(unique(do.call(rbind,lapply(metadata,nrow))))

    # Obtain variable codes for each row in metadata information
    varcode <- list()
    varname <- list()

    for(i in 1:nummeta){
      varcode <- append(varcode,
                        as.data.frame(unique(do.call(rbind,
                                                     lapply(metadata, '[',c(i),c("Variable.Id"))))))
      varname <- append(varname,
                        as.data.frame(unique(do.call(rbind,
                                                     lapply(metadata, '[',c(i),c("Variable.Nombre"))))))
    }

    if(request$addons$metacodes){
      metacols <- append(metacols, "Id")
      metacolsnames <- append(metacolsnames, ".Id")
    }

    # Loop through all variables
    k <- 1
    for(var in varcode){

      # Select a variable code and build a Unique dataframe of variable names
      dfcodes <- do.call(rbind,
                         lapply(metadata,
                                function(x) subset(x,
                                                   x$Variable.Id %in% var,
                                                   select = metacols)))

      # Rename column with variable code
      newname <- gsub("\\s+",".", varname[[k]][1])
      names(dfcodes) <- paste0(newname, metacolsnames)

      # Adding column to dataframe
      if(nrow(dfcodes) == nrow(dataout)){
        dataout <- cbind(dataout, dfcodes)
      }
      k <- k + 1
    }
  }

  return(dataout)
}

# Get metadata information about the variables and values present in a table
get_metadata_variable_values_table <- function(idTable, filter = NULL, verbose, validate, lang, progress = FALSE){

  # Check the type of the table
  checktable <- check_type_table(idTable = idTable, validate = validate, verbose = verbose, lang = lang)

  dfvalues <- NULL
  origin <- ""
  # Make sure the response is valid or null
  if(!check_result_status(checktable$groups)){

    # The table is in px or tpx format
    if(checktable$ispxtable){
      origin <- checktable$origin

      if(progress){
        cat(sprintf("- Processing filter: %s%%        \r", 50))
      }

      # Obtain metadata information
      df <- get_metadata_series_table(idTable = idTable, filter = filter, tip = "M", validate = FALSE, verbose = verbose, lang = lang)

      # Check if exits and id for variables
      existsvarid <- exists_variables_id(df$MetaData)

      # If exists variable's id and value's id add new origin and include ids in the selection
      if(exists_values_id(df$MetaData) && existsvarid$result) {
        selcol <- c("Nombre", "Codigo", "Id", "Variable.Nombre","Variable.Codigo", existsvarid$name)
        origin <- "tablepxid"

      }else{
        selcol <- c("Nombre", "Codigo", "Variable.Nombre","Variable.Codigo")
      }

      # Get the metadata with information of variables and values
      dfvalues <- lapply(df$MetaData,
                         function(x) subset(x, select = selcol))

      dfvalues <- unique(do.call(rbind, dfvalues))

      # The table is stored in tempus
    }else{
      origin <- checktable$origin

      i <- 1
      for(g in checktable$groups$Id){
        if(progress){
          cat(sprintf("- Processing filter: %s%%        \r", round(i/nrow(checktable$groups)*50,0)))
          i <- i + 1
        }

        df <- get_metadata_table_values(idTable = idTable, idGroup = g, validate = FALSE, lang = lang, verbose = verbose)
        df <- subset(df, select = c("Id", "Fk_Variable", "Nombre", "Codigo"))

        if (exists("dfvalues") && is.data.frame(get("dfvalues"))){
          dfvalues <- rbind(dfvalues,df)
        }else{
          dfvalues <- df
        }
      }
    }
  }

  return(list(origin = origin, values = dfvalues))
}

# Get metadata information about the variables and values present in an operation
get_metadata_variable_values_operation <- function(operation, verbose, validate, lang, progress = FALSE){

  dfvalues <- NULL

  # We obtain the variables from the operation of the series
  opevar <- get_metadata_variables(operation = operation, validate = validate, verbose = verbose, lang = lang, page = 0)

  # We obtain the values of all the variables
  i <- 1
  for(var in opevar$Id){
    if(progress){
      cat(sprintf("- Processing filter: %s%%        \r", round(i/nrow(opevar)*50,0)))
      i <- i + 1
    }

    tmp <- get_metadata_values(operation = operation, variable = var, validate = FALSE, verbose = verbose, lang = lang, page = 0)
    tmp <- subset(tmp, select = c("Id","Fk_Variable","Nombre","Codigo"))

    if (exists("dfvalues") && is.data.frame(get("dfvalues"))){
      dfvalues <- rbind(dfvalues,tmp)
    }else{
      dfvalues <- tmp
    }
  }

  return(list(origin = "series", values = dfvalues))
}



