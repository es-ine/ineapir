# Get data from a specific series

Retrieve data from series published by INE calling the API

## Usage

``` r
get_data_series(
  codSeries = NULL,
  nlast = 1,
  dateStart = NULL,
  dateEnd = NULL,
  det = 0,
  tip = NULL,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE,
  unnest = FALSE
)
```

## Arguments

- codSeries:

  (string): Code of the series. For further information about codes
  click this
  [link](https://es-ine.github.io/ineapir/articles/identify_codes.html).

- nlast:

  (int): number of periods to retrieve. By default is set to 1 period.

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

  (logical): validate input parameters.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

- unnest:

  (logical): set to TRUE to obtain a single data frame of data

## Value

Data frame with data of a series according to the code specified in the
function

## Examples

``` r
if (FALSE) { # interactive()
df <- get_data_series(codSeries = "IPC290750", nlast = 5)
head(df)

# Get data for an open range date
df <- get_data_series(codSeries = "IPC290750", dateStart = "2024/01/01")
head(df)

# Get data for a single range data
df <- get_data_series(codSeries = "IPC290750",
dateStart = "2023/01/01", dateEnd = "2023/05/01")
head(df)

# Get data for specific dates
df <- get_data_series(codSeries = "IPC290750",
dateStart = c("2023/01/01","2024/01/01"),
dateEnd = c("2023/01/01","2024/01/01"))
head(df)

# Get data for multiple date ranges
df <- get_data_series(codSeries = "IPC290750",
dateStart = c("2023/01/01","2024/01/01"),
dateEnd = c("2023/03/01","2024/03/01"))
head(df)
}
```
