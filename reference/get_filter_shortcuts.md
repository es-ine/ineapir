# Get all available filter shortcuts

Get all available filter shortcuts

## Usage

``` r
get_filter_shortcuts(lang = "ES", validate = TRUE, verbose = FALSE)
```

## Arguments

- lang:

  (string): language. Set to 'ES' for the Spanish version of the
  shortcuts or set to 'EN' for the English version of the shortcuts.

- validate:

  (logical): validate input parameters.

- verbose:

  (logical): print additional information.

## Value

Data frame with information of the available filter shortcuts

## Examples

``` r
# Shortcuts in spanish
df <- get_filter_shortcuts()
head(df)
#>   Shortcut Variable.ID                Comment
#> 1      nac         349               National
#> 2     prov         115              Provinces
#> 3     ccaa          70 Autonomous Communities
#> 4      mun          19         Municipalities
#> 5     isla          20                Islands
#> 7    grupo         762             CPI groups

# Shortcuts in english
df <- get_filter_shortcuts(lang = "EN")
head(df)
#>   Shortcut Variable.ID                Comment
#> 1      nac         349               National
#> 2     prov         115              Provinces
#> 3     ccaa          70 Autonomous Communities
#> 4      mun          19         Municipalities
#> 6   island          20                Islands
#> 8    group         762             CPI groups
```
