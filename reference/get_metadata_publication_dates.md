# Get the dates of a publication

Get the dates of a publication

## Usage

``` r
get_metadata_publication_dates(
  publication = NULL,
  det = 0,
  tip = NULL,
  lang = "ES",
  page = 0,
  validate = TRUE,
  verbose = FALSE
)
```

## Arguments

- publication:

  (int): id of the publication. To obtain a list of available
  publications see
  [`get_metadata_publications()`](https://inedifusion.github.io/ineapir/reference/get_metadata_publications.md).

- det:

  (int): level of detail. Valid values: 0, 1 or 2.

- tip:

  (string): set to 'A' for friendly output (e.g. readable dates), set to
  'M' to include metadata or set to 'AM' for both.

- lang:

  (string): language of the retrieved data. Set to 'ES' for Spanish or
  set to 'EN' for English.

- page:

  (int): page number. The retrieved result of the query is paginated
  (page=0 retrieves all pages).

- validate:

  (logical): validate input parameters. A FALSE value means fewer API
  calls.

- verbose:

  (logical): print additional information, including the URL to call the
  API service.

## Value

Data frame with information of the dates of the publication specified in
the function

## Examples

``` r
if (FALSE) { # interactive()
# Get the dates of a publication
df <- get_metadata_publication_dates(publication = 8, validate = FALSE)
head(df)
}
```
