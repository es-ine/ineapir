% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{get_metadata_variables}
\alias{get_metadata_variables}
\title{Get all available variables}
\usage{
get_metadata_variables(
  operation = NULL,
  lang = "ES",
  det = 0,
  page = 0,
  validate = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{operation}{(string): Code of the operation. Provide code to get all
the variables for the given operation. To obtain a list of
available operations see \code{\link[=get_metadata_operations]{get_metadata_operations()}}.
If no operation is specified then all the variables will be shown.}

\item{lang}{(string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.}

\item{det}{(int): level of detail. Valid values: 0, 1 or 2.}

\item{page}{(int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages).}

\item{validate}{(logical): validate input parameters. A FALSE value means fewer API calls.}

\item{verbose}{(logical): print additional information, including the URL to call the API service.}
}
\value{
Data frame with information of the available variables
}
\description{
Get all available variables
}
\examples{
\dontrun{
get_metadata_variables()
get_metadata_variables(operation = "IPC")
}

}
