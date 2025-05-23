% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_series.R
\name{get_metadata_series_varval}
\alias{get_metadata_series_varval}
\title{Get metadata information about the variables and values of series for a given operation}
\usage{
get_metadata_series_varval(
  operation = NULL,
  lang = "ES",
  det = 0,
  validate = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{operation}{(string): code of the operation. To obtain a list of
available operations see \code{\link[=get_metadata_operations]{get_metadata_operations()}}.}

\item{lang}{(string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.}

\item{det}{(int): level of detail. Valid values: 0, 1 or 2.}

\item{validate}{(logical): validate input parameters. A FALSE value means fewer API calls.}

\item{verbose}{(logical): print additional information, including the URL to call the API service.}
}
\value{
Data frame with information about the variables and values that
define the series according to the operation specified in the function
}
\description{
Get metadata information about the variables and values of series for a given operation
}
\examples{
\dontrun{
get_metadata_series_varval(operation = "IPC")
}

}
