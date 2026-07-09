# Tips

## Use of `dateStart` and `dateEnd`

The arguments `dateStart` and `dateEnd` are available in
[`get_data_table()`](https://inedifusion.github.io/ineapir/reference/get_data_table.md),
[`get_data_series()`](https://inedifusion.github.io/ineapir/reference/get_data_series.md)
and
[`get_data_series_filter()`](https://inedifusion.github.io/ineapir/reference/get_data_series_filter.md),
and allow you to filter data by date.

- Single date:

``` r

library(ineapir)
filter <- list("3" = "74" ,     # variable id = 3, value id = 74
               "762" = "304092" # variable id = 762, value id = 304092
               )

# Request data using dates. The required format is yyyy/mm/dd.
ipc <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", validate = FALSE,
                      dateStart = "2025/01/01", dateEnd = "2025/01/01")
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                        Nombre T3_Periodo Anyo Valor
#> 1 Nacional. Índice general. Variación anual.         M01 2025   2.9
```

- Single range of dates:

``` r

# Request data using dates. The required format is yyyy/mm/dd.
ipc <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", validate = FALSE,
                      dateStart = "2024/01/01", dateEnd = "2024/12/01")
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                           Nombre T3_Periodo Anyo Valor
#> 1    Nacional. Índice general. Variación anual.         M12 2024   2.8
#> 1.1  Nacional. Índice general. Variación anual.         M11 2024   2.4
#> 1.2  Nacional. Índice general. Variación anual.         M10 2024   1.8
#> 1.3  Nacional. Índice general. Variación anual.         M09 2024   1.5
#> 1.4  Nacional. Índice general. Variación anual.         M08 2024   2.3
#> 1.5  Nacional. Índice general. Variación anual.         M07 2024   2.8
#> 1.6  Nacional. Índice general. Variación anual.         M06 2024   3.4
#> 1.7  Nacional. Índice general. Variación anual.         M05 2024   3.6
#> 1.8  Nacional. Índice general. Variación anual.         M04 2024   3.3
#> 1.9  Nacional. Índice general. Variación anual.         M03 2024   3.2
#> 1.10 Nacional. Índice general. Variación anual.         M02 2024   2.8
#> 1.11 Nacional. Índice general. Variación anual.         M01 2024   3.4
```

- Open range of dates:

``` r

# Request data using dates. The required format is yyyy/mm/dd.
ipc <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", validate = FALSE,
                      dateStart = "2025/01/01")
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                           Nombre T3_Periodo Anyo Valor
#> 1    Nacional. Índice general. Variación anual.         M06 2026   3.2
#> 1.1  Nacional. Índice general. Variación anual.         M05 2026   3.2
#> 1.2  Nacional. Índice general. Variación anual.         M04 2026   3.2
#> 1.3  Nacional. Índice general. Variación anual.         M03 2026   3.4
#> 1.4  Nacional. Índice general. Variación anual.         M02 2026   2.3
#> 1.5  Nacional. Índice general. Variación anual.         M01 2026   2.3
#> 1.6  Nacional. Índice general. Variación anual.         M12 2025   2.9
#> 1.7  Nacional. Índice general. Variación anual.         M11 2025   3.0
#> 1.8  Nacional. Índice general. Variación anual.         M10 2025   3.1
#> 1.9  Nacional. Índice general. Variación anual.         M09 2025   3.0
#> 1.10 Nacional. Índice general. Variación anual.         M08 2025   2.7
#> 1.11 Nacional. Índice general. Variación anual.         M07 2025   2.7
#> 1.12 Nacional. Índice general. Variación anual.         M06 2025   2.3
#> 1.13 Nacional. Índice general. Variación anual.         M05 2025   2.0
#> 1.14 Nacional. Índice general. Variación anual.         M04 2025   2.2
#> 1.15 Nacional. Índice general. Variación anual.         M03 2025   2.3
#> 1.16 Nacional. Índice general. Variación anual.         M02 2025   3.0
#> 1.17 Nacional. Índice general. Variación anual.         M01 2025   2.9
```

- Multiple dates:

``` r

# Request data using dates. The required format is yyyy/mm/dd.
ipc <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", validate = FALSE,
                      dateStart = c("2023/01/01", "2024/01/01", "2025/01/01"), 
                      dateEnd = c("2023/01/01", "2024/01/01", "2025/01/01"))
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                          Nombre T3_Periodo Anyo Valor
#> 1   Nacional. Índice general. Variación anual.         M01 2025   2.9
#> 1.1 Nacional. Índice general. Variación anual.         M01 2024   3.4
#> 1.2 Nacional. Índice general. Variación anual.         M01 2023   5.9
```

- Multiple range of dates:

``` r

# Request data using dates. The required format is yyyy/mm/dd.
ipc <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", validate = FALSE,
                      dateStart = c("2023/01/01", "2024/01/01", "2025/01/01"),
                      dateEnd = c("2023/03/01", "2024/03/01", "2025/03/01"))
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                          Nombre T3_Periodo Anyo Valor
#> 1   Nacional. Índice general. Variación anual.         M03 2025   2.3
#> 1.1 Nacional. Índice general. Variación anual.         M02 2025   3.0
#> 1.2 Nacional. Índice general. Variación anual.         M01 2025   2.9
#> 1.3 Nacional. Índice general. Variación anual.         M03 2024   3.2
#> 1.4 Nacional. Índice general. Variación anual.         M02 2024   2.8
#> 1.5 Nacional. Índice general. Variación anual.         M01 2024   3.4
#> 1.6 Nacional. Índice general. Variación anual.         M03 2023   3.3
#> 1.7 Nacional. Índice general. Variación anual.         M02 2023   6.0
#> 1.8 Nacional. Índice general. Variación anual.         M01 2023   5.9
```

- Multiple range of dates and a final open range:

``` r

# Request data using dates. The required format is yyyy/mm/dd.
ipc <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", validate = FALSE,
                      dateStart = c("2023/01/01", "2024/01/01", "2025/01/01"), 
                      dateEnd = c("2023/03/01", "2024/03/01"))
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                           Nombre T3_Periodo Anyo Valor
#> 1    Nacional. Índice general. Variación anual.         M06 2026   3.2
#> 1.1  Nacional. Índice general. Variación anual.         M05 2026   3.2
#> 1.2  Nacional. Índice general. Variación anual.         M04 2026   3.2
#> 1.3  Nacional. Índice general. Variación anual.         M03 2026   3.4
#> 1.4  Nacional. Índice general. Variación anual.         M02 2026   2.3
#> 1.5  Nacional. Índice general. Variación anual.         M01 2026   2.3
#> 1.6  Nacional. Índice general. Variación anual.         M12 2025   2.9
#> 1.7  Nacional. Índice general. Variación anual.         M11 2025   3.0
#> 1.8  Nacional. Índice general. Variación anual.         M10 2025   3.1
#> 1.9  Nacional. Índice general. Variación anual.         M09 2025   3.0
#> 1.10 Nacional. Índice general. Variación anual.         M08 2025   2.7
#> 1.11 Nacional. Índice general. Variación anual.         M07 2025   2.7
#> 1.12 Nacional. Índice general. Variación anual.         M06 2025   2.3
#> 1.13 Nacional. Índice general. Variación anual.         M05 2025   2.0
#> 1.14 Nacional. Índice general. Variación anual.         M04 2025   2.2
#> 1.15 Nacional. Índice general. Variación anual.         M03 2025   2.3
#> 1.16 Nacional. Índice general. Variación anual.         M02 2025   3.0
#> 1.17 Nacional. Índice general. Variación anual.         M01 2025   2.9
#> 1.18 Nacional. Índice general. Variación anual.         M03 2024   3.2
#> 1.19 Nacional. Índice general. Variación anual.         M02 2024   2.8
#> 1.20 Nacional. Índice general. Variación anual.         M01 2024   3.4
#> 1.21 Nacional. Índice general. Variación anual.         M03 2023   3.3
#> 1.22 Nacional. Índice general. Variación anual.         M02 2023   6.0
#> 1.23 Nacional. Índice general. Variación anual.         M01 2023   5.9
```

## Use of `verbose`

When the argument `verbose = TRUE` is passed to a function, a number of
information is displayed in the console, including the URL used to call
the API.

``` r

# Variables used in the operation IPC and verbose = TRUE
variables <- get_metadata_variables(operation = "IPC", verbose = TRUE)
#> - Check lang: OK
#> - API URL: https://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES?page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES?page=2&ver=3
#> - Check operation: OK
#> - Check det: OK
#> - Check page: OK
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/IPC?det=0&page=2&ver=3
```

## Use of `validate`

If it is not necessary to validate the values of the arguments passed to
a function, this validation can be turned off with `validate = FALSE`.

``` r

# Variables used in the operation IPC with verbose = TRUE and validate = FALSE
variables <- get_metadata_variables(operation = "IPC", verbose = TRUE,
                                    validate = FALSE)
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/IPC?det=0&page=2&ver=3
```

This is useful when we are filtering data and the filter we are using is
the one we want. Turning off validation means making fewer API calls.
For better performance, it is recommended to use codes in the filter
instead of shortcuts and disable validation if we are sure that the
filter is correct.

``` r

# The filter 
filter <- list("115" = "2",     # variable id = 115, value id = 2
               "3" = "74" ,     # variable id = 3, value id = 74
               "762" = "304092" # variable id = 762, value id = 304092
               )

# Request data using the filter with verbose = TRUE and validate = TRUE
ipc <- get_data_series_filter(operation = "IPC", filter = filter, periodicity = 1,
                       verbose = TRUE)
#> - Check lang: OK
#> - API URL: https://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES?page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/OPERACIONES_DISPONIBLES?page=2&ver=3
#> - Check operation: OK
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VARIABLES_OPERACION/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/3/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/3/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/70/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/70/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/115/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/115/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/269/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/269/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/270/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/270/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/349/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/349/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/544/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/544/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/762/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/762/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/763/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/763/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/764/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/764/IPC?det=0&page=2&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/765/IPC?det=0&page=1&ver=3
#> - API URL: https://servicios.ine.es/wstempus/js/ES/VALORES_VARIABLEOPERACION/765/IPC?det=0&page=2&ver=3
#> - Check filter: OK
#> - API URL: https://servicios.ine.es/wstempus/js/ES/PERIODICIDAD_OPERACION/IPC?ver=3
#> - Check periodicity: OK
#> - Check nlast: OK
#> - Check det: OK
#> - Check page: OK
#> - Processing filter: 0% - Processing filter: 67%        - Processing filter: 83%        - Processing filter: 100%        - Processing filter: 100%         
#> - API URL: https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?p=1&nult=1&det=0&page=1&ver=3&g1=115%3A2&g2=3%3A74&g3=762%3A304092

# Request data using the filter with verbose = TRUE and validate = FALSE
ipc <- get_data_series_filter(operation = "IPC", filter = filter, periodicity = 1,
                       verbose = TRUE, validate = FALSE)
#> - Processing filter: 0% - Processing filter: 67%        - Processing filter: 83%        - Processing filter: 100%        - Processing filter: 100%         
#> - API URL: https://servicios.ine.es/wstempus/js/ES/DATOS_METADATAOPERACION/IPC?p=1&nult=1&det=0&page=1&ver=3&g1=115%3A2&g2=3%3A74&g3=762%3A304092
```

## Use of `lang`

It is possible to retrieve information in English passing the argument
`lang = "EN"` to certain functions.

``` r

# Variables used in the operation IPC and lang = "EN"
variables <- get_metadata_variables(operation = "IPC", lang = "EN")
variables
#>     Id                            Nombre Codigo
#> 1    3                      Type of data       
#> 2   70 Autonomous Communities and Cities   CCAA
#> 3  115                         Provinces   PROV
#> 4  269               Special groups 2001       
#> 5  270                     Headings 2001       
#> 6  349                    National Total    NAC
#> 7  544             Correction of effects       
#> 8  762                    ECOICOP Groups       
#> 9  763                ECOICOP sub-groups       
#> 10 764                   ECOICOP classes       
#> 11 765                ECOICOP subclasses
```

We can use the English language as well filtering data using shortcuts.

``` r

# Filter with the values wrapper (CPI)
filter <- list(values = c("annual change" , "overall index"))

# Request data using the filter with shortcut = TRUE and lang = "EN"
cpi <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", nlast = 5, lang = "EN")

cpi[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                       Nombre T3_Periodo Anyo Valor
#> 1   National. Overall index. Annual change.         M06 2026   3.2
#> 1.1 National. Overall index. Annual change.         M05 2026   3.2
#> 1.2 National. Overall index. Annual change.         M04 2026   3.2
#> 1.3 National. Overall index. Annual change.         M03 2026   3.4
#> 1.4 National. Overall index. Annual change.         M02 2026   2.3
```

## Use of `unnest`

When we request data from tables or series using the functions
[`get_data_table()`](https://inedifusion.github.io/ineapir/reference/get_data_table.md),
[`get_data_series()`](https://inedifusion.github.io/ineapir/reference/get_data_series.md)
and `get_data_series_filter`, the `$Data` column containing the data
values is a list of data frames, one for each entry.

``` r

# Filter with the values wrapper (CPI)
filter <- list(values = c("variación anual" , "variación mensual", "índice general"))

# Request data using the filter and unnest = FALSE
ipc <- get_data_table(idTable = 76125, filter = filter, tip = "A", nlast = 3)

# We have two entries
 ipc[,c("COD","Nombre")]
#>         COD                                        Nombre
#> 1 IPC290752 Nacional. Índice general. Variación mensual. 
#> 2 IPC290750   Nacional. Índice general. Variación anual.
 
# Data column is a list of two data frames. One for each entry. 
 ipc$Data
#> [[1]]
#>                           Fecha T3_TipoDato T3_Periodo Anyo Valor
#> 1 2026-06-01T00:00:00.000+02:00      Avance        M06 2026   0.6
#> 2 2026-05-01T00:00:00.000+02:00  Definitivo        M05 2026   0.1
#> 3 2026-04-01T00:00:00.000+02:00  Definitivo        M04 2026   0.4
#> 
#> [[2]]
#>                           Fecha T3_TipoDato T3_Periodo Anyo Valor
#> 1 2026-06-01T00:00:00.000+02:00      Avance        M06 2026   3.2
#> 2 2026-05-01T00:00:00.000+02:00  Definitivo        M05 2026   3.2
#> 3 2026-04-01T00:00:00.000+02:00  Definitivo        M04 2026   3.2
```

If we want to get only one data frame for all data values, we can pass
the argument `unnest = TRUE`.

``` r

# Filter with the values wrapper (CPI)
filter <- list(values = c("variación anual" , "variación mensual", "índice general"))

# Request data using the filter and unnest = TRUE
ipc <- get_data_table(idTable = 76125, filter = filter, tip = "A",
                      nlast = 3, unnest = TRUE)

# Unique data frame with 6 rows
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                            Nombre T3_Periodo Anyo Valor
#> 1   Nacional. Índice general. Variación mensual.         M06 2026   0.6
#> 1.1 Nacional. Índice general. Variación mensual.         M05 2026   0.1
#> 1.2 Nacional. Índice general. Variación mensual.         M04 2026   0.4
#> 2     Nacional. Índice general. Variación anual.         M06 2026   3.2
#> 2.1   Nacional. Índice general. Variación anual.         M05 2026   3.2
#> 2.2   Nacional. Índice general. Variación anual.         M04 2026   3.2
```

## Use of `metanames` and `metacodes`

When we pass the argument `metanames = TRUE` we get additional columns
that contain the names of the values extracted from the metadata
information. To get additional columns containing the codes and ids of
the values extracted from the metadata information we pass the argument
`metacodes = TRUE`.

``` r

# Filter for total population of men and women 
filter <- list(nacionalidad = "total", sexo = c("hombres", "mujeres"))

# Table: Resident population by date, sex, age group and nationality
poblacion <- get_data_table(idTable = 9674, filter = filter, validate = FALSE, 
                            tip = "AM", unnest = TRUE, nlast = 1)
# Names of columns
names(poblacion)
#>  [1] "COD"         "Nombre"      "T3_Unidad"   "T3_Escala"   "MetaData"   
#>  [6] "Fecha"       "T3_TipoDato" "T3_Periodo"  "Anyo"        "Valor"

# Request data with metanames = TRUE
poblacion <- get_data_table(idTable = 9674, filter = filter, validate = FALSE, 
                            tip = "AM", unnest = TRUE, nlast = 1,
                            metanames =  TRUE, metacodes = TRUE)

# Additional columns of Nacionalidad, Edad and Sexo
names(poblacion)
#>  [1] "COD"                             "Nombre"                         
#>  [3] "T3_Unidad"                       "T3_Escala"                      
#>  [5] "MetaData"                        "Nacionalidad"                   
#>  [7] "Nacionalidad.Codigo"             "Nacionalidad.Id"                
#>  [9] "Grupo.quinquenal.de.edad"        "Grupo.quinquenal.de.edad.Codigo"
#> [11] "Grupo.quinquenal.de.edad.Id"     "Sexo"                           
#> [13] "Sexo.Codigo"                     "Sexo.Id"                        
#> [15] "Fecha"                           "T3_TipoDato"                    
#> [17] "T3_Periodo"                      "Anyo"                           
#> [19] "Valor"
```

## Filtering with regular expresions

When we use shortcuts names in the filter, we can also include as values
regular expressions if this helps us filter the data better.

``` r

# Filter with a regular expression for ages between 20 and 25 years old
filter <- list(sexo = "total", edad = "2[0-5]+")

# Request data using the filter with a regular expression
pob <- get_data_table(idTable = 56934, filter = filter, tip = "A", nlast = 1,
                      unnest = TRUE)

# Only ages between 20 and 25 years old
pob[,c("Nombre", "Anyo", "Valor")]
#>                                                Nombre Anyo  Valor
#> 1 Total Nacional. 20 años. Total. Población. Número.  2025 542720
#> 2 Total Nacional. 21 años. Total. Población. Número.  2025 541011
#> 3 Total Nacional. 22 años. Total. Población. Número.  2025 530297
#> 4 Total Nacional. 23 años. Total. Población. Número.  2025 534266
#> 5 Total Nacional. 24 años. Total. Población. Número.  2025 545337
#> 6 Total Nacional. 25 años. Total. Población. Número.  2025 538328
```
