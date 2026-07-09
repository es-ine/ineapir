# How to filter data

When we request data from tables or series there is the possibility of
filtering data on the fly using metadata information about the variables
and their values that define the series.

## Filtering data from tables

It is necessary to pass the argument `filter` of the
[`get_data_table()`](https://inedifusion.github.io/ineapir/reference/get_data_table.md)
function, which is a [`list()`](https://rdrr.io/r/base/list.html) of
variables and the values they take. There are two approaches to build
the filter depending on the table type.

### Case one.

- **URL:** <https://www.ine.es/jaxiT3/Tabla.htm?t=76125>

1.  The first step is to obtain the values of the groups (combo boxes)
    of the table that interest us to build the filter.

``` r

library(ineapir)

# Get metadata information of the table
metadata <- get_metadata_table_varval(idTable = 76125, validate = FALSE)
head(metadata,4)
#>       Id Fk_Variable                             Nombre Codigo
#> 1 304092         762                     Índice general     00
#> 2 304093         762 Alimentos y bebidas no alcohólicas     01
#> 3 418050         762       Bebidas alcohólicas y tabaco     02
#> 4 304095         762                  Vestido y calzado     03
tail(metadata,4)
#>    Id Fk_Variable                        Nombre Codigo
#> 15 83           3                        Índice      0
#> 16 84           3             Variación mensual      1
#> 17 74           3               Variación anual      2
#> 18 87           3 Variación en lo que va de año      5
```

2.  With this information we build the filter as follows.

``` r

# The filter is a list()
filter <- list("3" = "74" ,     # variable id = 3, value id = 74
               "762" = "304092" # variable id = 762, value id = 304092
               )

# Request data using the filter
ipc <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", nlast = 5, validate = FALSE)
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                          Nombre T3_Periodo Anyo Valor
#> 1   Nacional. Índice general. Variación anual.         M06 2026   3.2
#> 1.1 Nacional. Índice general. Variación anual.         M05 2026   3.2
#> 1.2 Nacional. Índice general. Variación anual.         M04 2026   3.2
#> 1.3 Nacional. Índice general. Variación anual.         M03 2026   3.4
#> 1.4 Nacional. Índice general. Variación anual.         M02 2026   2.3
```

- **NOTE** If we add a minus sign to a value, the value will be excluded
  from the filter.

``` r

# The filter is a list()
filter <- list("3" = "74" ,     # variable id = 3, value id = 74
               "762" = "-304092" # variable id = 762, value id = 304092
               )

# Request data using the filter
ipc <- get_data_table(idTable = 76125, filter = filter, unnest = TRUE,
                      tip = "A", nlast = 2, validate = FALSE)
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                                                                                             Nombre
#> 1                                                  Nacional. Alimentos y bebidas no alcohólicas. Variación anual. 
#> 2                                                        Nacional. Bebidas alcohólicas y tabaco. Variación anual. 
#> 3                                                                   Nacional. Vestido y calzado. Variación anual. 
#> 4                              Nacional. Vivienda, agua, electricidad, gas y otros combustibles. Variación anual. 
#> 5  Nacional. Muebles, artículos del hogar y artículos para el mantenimiento corriente del hogar. Variación anual. 
#> 6                                                                             Nacional. Sanidad. Variación anual. 
#> 7                                                                          Nacional. Transporte. Variación anual. 
#> 8                                                        Nacional. Información y comunicaciones. Variación anual. 
#> 9                                          Nacional. Actividades recreativas, deporte y cultura. Variación anual. 
#> 10                                                                          Nacional. Enseñanza. Variación anual. 
#> 11                                            Nacional. Restaurantes y servicios de alojamiento. Variación anual. 
#> 12                                                    Nacional. Seguros y servicios financieros. Variación anual. 
#> 13                 Nacional. Cuidado personal, protección social, y bienes y servicios diversos. Variación anual. 
#>    T3_Periodo Anyo Valor
#> 1         M05 2026   2.2
#> 2         M05 2026   3.5
#> 3         M05 2026  -1.1
#> 4         M05 2026   1.4
#> 5         M05 2026   1.2
#> 6         M05 2026   2.1
#> 7         M05 2026   7.4
#> 8         M05 2026   0.2
#> 9         M05 2026   2.6
#> 10        M05 2026   2.6
#> 11        M05 2026   4.8
#> 12        M05 2026   4.6
#> 13        M05 2026   3.1
```

### Case two (pc-axis file)

- **URL:**
  <https://www.ine.es/jaxi/Tabla.htm?path=/t20/e245/p08/l0/&file=01001.px>

1.  The first step is to obtain the metadata information from a table.

``` r

# Get metadata information of the table
metadata <- get_metadata_table_varval(idTable = "t20/e245/p08/l0/01001.px")
metadata
#>            Nombre        Codigo         Variable.Nombre      Variable.Codigo
#> 1    TOTAL EDADES   totaledades edad (3 grupos de edad)    edad3gruposdeedad
#> 2           TOTAL         total   Españoles/Extranjeros espanolesextranjeros
#> 3     Ambos sexos    ambossexos                    Sexo                 sexo
#> 6         Hombres       hombres                    Sexo                 sexo
#> 9         Mujeres       mujeres                    Sexo                 sexo
#> 11      Españoles     espanoles   Españoles/Extranjeros espanolesextranjeros
#> 20    Extranjeros   extranjeros   Españoles/Extranjeros espanolesextranjeros
#> 29  % Extranjeros extranjeros~1   Españoles/Extranjeros espanolesextranjeros
#> 37      0-15 años       015anos edad (3 grupos de edad)    edad3gruposdeedad
#> 73     16-64 años      1664anos edad (3 grupos de edad)    edad3gruposdeedad
#> 109      65 y más        65ymas edad (3 grupos de edad)    edad3gruposdeedad
```

``` r

# NOTE: for px tables we can use a filter.
metadata <- get_metadata_table_varval(idTable = "t20/e245/p08/l0/01001.px",
                                      filter = list(sexo = "ambossexos"))
metadata
#>           Nombre        Codigo         Variable.Nombre      Variable.Codigo
#> 1   TOTAL EDADES   totaledades edad (3 grupos de edad)    edad3gruposdeedad
#> 2          TOTAL         total   Españoles/Extranjeros espanolesextranjeros
#> 3    Ambos sexos    ambossexos                    Sexo                 sexo
#> 5      Españoles     espanoles   Españoles/Extranjeros espanolesextranjeros
#> 8    Extranjeros   extranjeros   Españoles/Extranjeros espanolesextranjeros
#> 11 % Extranjeros extranjeros~1   Españoles/Extranjeros espanolesextranjeros
#> 13     0-15 años       015anos edad (3 grupos de edad)    edad3gruposdeedad
#> 25    16-64 años      1664anos edad (3 grupos de edad)    edad3gruposdeedad
#> 37      65 y más        65ymas edad (3 grupos de edad)    edad3gruposdeedad
```

2.  With this information we build the filter as follows.

``` r

# Build the filter with the codes of variables and values
filter <- list(sexo = "ambossexos",              
               espanolesextranjeros = "total",   
               edad3gruposdeedad = "totaledades" 
               ) 

# Request data using the filter
poblacion <- get_data_table(idTable = "t20/e245/p08/l0/01001.px", unnest = TRUE,
                            tip = "A", nlast = 5, filter = filter,
                            validate = FALSE)
poblacion
#>                             Nombre NombrePeriodo    Valor
#> 1 TOTAL EDADES, TOTAL, Ambos sexos          2022 47475420
#> 2 TOTAL EDADES, TOTAL, Ambos sexos          2021 47385107
#> 3 TOTAL EDADES, TOTAL, Ambos sexos          2020 47450795
#> 4 TOTAL EDADES, TOTAL, Ambos sexos          2019 47026208
#> 5 TOTAL EDADES, TOTAL, Ambos sexos          2018 46722980
```

- **NOTE** If we add a minus sign to a value, the value will be excluded
  from the filter.

``` r

# Build the filter with the codes of variables and values
filter <- list(sexo = "-ambossexos",              
               espanolesextranjeros = "total",   
               edad3gruposdeedad = "totaledades" 
               ) 

# Request data using the filter
poblacion <- get_data_table(idTable = "t20/e245/p08/l0/01001.px", unnest = TRUE,
                            tip = "A", nlast = 1, filter = filter,
                            validate = FALSE)
#> An error occurred calling the API (status 404).
#> https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/t20/e245/p08/l0/01001.px?nult=1&det=0&tip=A&ver=3&tv=sexo%3Ahombres&tv=sexo%3Amujeres&tv=espanolesextranjeros%3Atotal&tv=edad3gruposdeedad%3Atotaledades
poblacion
#> NULL
```

### Case three (tpx file)

- **URL:** <https://www.ine.es/jaxi/Tabla.htm?tpx=33387&L=0>

1.  The first step is to obtain the metadata information from a table.

``` r

# Get metadata information of the table
metadata <- get_metadata_table_varval(idTable = 33387)
#> An error occurred calling the API (status 404).
#> https://servicios.ine.es/wstempus/jsCache/ES/SERIES_TABLA/33387?det=0&tip=M&ver=3
metadata[grepl("^\\d{1}\\D+",metadata$Codigo),]
#> NULL
```

``` r

# NOTE: for tpx tables we can use a filter.
metadata <- get_metadata_table_varval(idTable = 33387,
                                      filter = list(tipodematerial = "1biomasa"))
#> An error occurred calling the API (status 404).
#> https://servicios.ine.es/wstempus/jsCache/ES/SERIES_TABLA/33387?det=0&tip=M&ver=3
metadata[grepl("^\\d{1}\\D+",metadata$Codigo),]
#>        Nombre   Codigo  Variable.Nombre Variable.Codigo
#> 1 1.  Biomasa 1biomasa tipo de material  tipodematerial
```

2.  With this information we build the filter as follows.

``` r

# Build the filter with the codes of variables and values
# A variable can take more than one value
filter <- list(tipodematerial = c("1biomasa", "2mineralesmetalicosmineralenbruto",
                                  "3mineralesnometalicos", "4combustiblesfosiles")
               )


# Request data using the filter
materiales <- get_data_table(idTable = 33387, unnest = TRUE, tip = "A",
                             nlast = 1, filter = filter, validate = FALSE)
#> An error occurred calling the API (status 404).
#> https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/33387?nult=1&det=0&tip=A&ver=3&tv=tipodematerial%3A1biomasa&tv=tipodematerial%3A2mineralesmetalicosmineralenbruto&tv=tipodematerial%3A3mineralesnometalicos&tv=tipodematerial%3A4combustiblesfosiles
materiales
#> NULL
```

- **NOTE** If we add a minus sign to a value, the value will be excluded
  from the filter.

``` r

# Build the filter with the codes of variables and values
# A variable can take more than one value
filter <- list(tipodematerial = c("-1biomasa", "-2mineralesmetalicosmineralenbruto",
                                  "-3mineralesnometalicos", "-4combustiblesfosiles")
               )


# Request data using the filter
materiales <- get_data_table(idTable = 33387, unnest = TRUE, tip = "A",
                             nlast = 1, filter = filter, validate = FALSE)
#> An error occurred calling the API (status 404).
#> https://servicios.ine.es/wstempus/jsCache/ES/SERIES_TABLA/33387?det=0&tip=M&ver=3
head(materiales)
#>                        Nombre NombrePeriodo     Valor
#> 1         EXTRACCION NACIONAL 2024 (avance) 371995244
#> 2                 1.  Biomasa 2024 (avance) 126108212
#> 3    1.1.  Cultivos primarios 2024 (avance)  67456391
#> 4            1.1.1.  Cereales 2024 (avance)        NA
#> 5 1.1.2.  Raices y tubérculos 2024 (avance)        NA
#> 6 1.1.3.  Cultivos azucareros 2024 (avance)        NA
```

- **URL:** <https://www.ine.es/jaxi/Tabla.htm?tpx=52056&L=0>

1.  There are tpx tables that contain variable ids and value ids. We can
    see that when we obtain the metadata information from a table.

``` r

# Get metadata information of the table using a filter.
# The filter is useful when the table contains a large number of series
# (for example, tables with a large territorial segmentation) which slows down
# the information retrieval.
metadata <- get_metadata_table_varval(idTable = 52056,
                                      filter = list(NAC = "00"))
head(metadata[order(metadata$Variable.Id),],4)
#>       Id      Nombre Codigo Variable.Id Variable.Nombre Variable.Codigo
#> 6  11406       Valor                  3    Tipo de dato                
#> 12    77  Porcentaje                  3    Tipo de dato                
#> 4    451 Ambos sexos                 18            Sexo                
#> 40   452     Hombres      1          18            Sexo
```

2.  In this case, we can use the ids instead of the codes to build the
    filter. To do this we add the alias *~id* at the end of each id.

``` r

# In order to use the ids of variables and values we add the alias '~id'
filter = list("349~id" = "16473~id",  # variable id = 349, value id = 16473
              "916~id" = "391871~id", # variable id = 909, value id = 391455
              "942~id" = "274282~id", # variable id = 942, value id = 274282
              "999~id" = "391770~id", # variable id = 975, value id = 391438
              "3~id"   = "11406~id"  # variable id = 3, value id = 11406
              )

# Request data using the filter
explotaciones <- get_data_table(idTable = 52056, unnest = TRUE, tip = "A",
                             nlast = 1, filter = filter, validate = FALSE)
explotaciones
#>                                                                                     Nombre
#> 1 Total Nacional, Total tramos UTAT, Total mano de obra, Ambos sexos, Explotaciones, Valor
#> 2     Total Nacional, Total tramos UTAT, Total mano de obra, Hombres, Explotaciones, Valor
#> 3     Total Nacional, Total tramos UTAT, Total mano de obra, Mujeres, Explotaciones, Valor
#>    Valor
#> 1 894718
#> 2 757886
#> 3 356571
```

## Filtering data from series

It is necessary to pass the argument `filter` of the
[`get_data_series_filter()`](https://inedifusion.github.io/ineapir/reference/get_data_series_filter.md)
function, which is a [`list()`](https://rdrr.io/r/base/list.html) of
variables and the values they take.

1.  The first step is to obtain the variables used in the operation to
    which the series belong.

``` r

# Variables used in the operation IPC
variables <- get_metadata_variables(operation = "IPC", validate = FALSE)
variables
#>     Id                           Nombre Codigo
#> 1    3                     Tipo de dato       
#> 2   70 Comunidades y Ciudades Autónomas   CCAA
#> 3  115                       Provincias   PROV
#> 4  269           Grupos especiales 2001       
#> 5  270                    Rúbricas 2001       
#> 6  349                   Total Nacional    NAC
#> 7  544            Corrección de efectos       
#> 8  762                   Grupos ECOICOP       
#> 9  763                Subgrupos ECOICOP       
#> 10 764                   Clases ECOICOP       
#> 11 765                Subclases ECOICOP
```

2.  The second step is to obtain the values of the variables that
    interest us to build the filter.

``` r

# Values of the variable with id = 115
provincias <- get_metadata_values(operation = "IPC", variable = 115, validate = FALSE)
head(provincias)
#>   Id Fk_Variable           Nombre Codigo FK_JerarquiaPadres
#> 1  2         115      Araba/Álava     01               9012
#> 2  3         115         Albacete     02               9004
#> 3  4         115 Alicante/Alacant     03               9006
#> 4  5         115          Almería     04               8997
#> 5  6         115            Ávila     05               9003
#> 6  7         115          Badajoz     06               9007

# Values of the variable with id = 3
tipo <- get_metadata_values(operation = "IPC", variable = 3, validate = FALSE)
head(tipo)
#>   Id Fk_Variable            Nombre Codigo
#> 1 72           3         Dato base       
#> 2 74           3   Variación anual       
#> 3 83           3            Índice       
#> 4 84           3 Variación mensual       
#> 5 85           3       Media anual      M
#> 6 86           3   Variación anual

# Values of the variable with id = 762
grupos <- get_metadata_values(operation = "IPC", variable = 762, validate = FALSE)
head(grupos, 4)
#>       Id Fk_Variable                             Nombre Codigo
#> 1 304092         762                     Índice general     00
#> 2 304093         762 Alimentos y bebidas no alcohólicas     01
#> 3 304094         762       Bebidas alcohólicas y tabaco     02
#> 4 304095         762                  Vestido y calzado     03
#>   FK_JerarquiaPadres
#> 1               NULL
#> 2             304092
#> 3             304092
#> 4             304092

# We can get all the values at once with the function get_metadata_series_varval
varval <- get_metadata_series_varval(operation = "IPC", validate = FALSE)
head(subset(varval, Fk_Variable == 115))
#>    Id Fk_Variable           Nombre Codigo
#> 33  2         115      Araba/Álava     01
#> 34  3         115         Albacete     02
#> 35  4         115 Alicante/Alacant     03
#> 36  5         115          Almería     04
#> 37  6         115            Ávila     05
#> 38  7         115          Badajoz     06
head(subset(varval, Fk_Variable == 3))
#>   Id Fk_Variable            Nombre Codigo
#> 1 72           3         Dato base       
#> 2 74           3   Variación anual       
#> 3 83           3            Índice       
#> 4 84           3 Variación mensual       
#> 5 85           3       Media anual      M
#> 6 86           3   Variación anual
head(subset(varval, Fk_Variable == 762), 4)
#>         Id Fk_Variable                             Nombre Codigo
#> 179 304092         762                     Índice general     00
#> 180 304093         762 Alimentos y bebidas no alcohólicas     01
#> 181 304094         762       Bebidas alcohólicas y tabaco     02
#> 182 304095         762                  Vestido y calzado     03
```

3.  With this information we build the filter as follows.

``` r

# The filter is a list()
filter <- list("115" = "2",     # variable id = 115, value id = 2
               "3" = "74" ,     # variable id = 3, value id = 74
               "762" = "304092" # variable id = 762, value id = 304092
               )

# Request data using the filter
ipc <- get_data_series_filter(operation = "IPC", filter = filter, periodicity = 1,
                              unnest = TRUE, tip = "A", validate = FALSE)
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                           Nombre T3_Periodo Anyo Valor
#> 1 Araba/Álava. Índice general. Variación anual.        may. 2026   3.3
```

- A variable can take more than one value (valid to filter data from
  tables as well).

``` r

# The filter is a list()
filter <- list("115" = c("2" ,"3", "4"), # variable id = 115, values id 2, 3, 4
               "3" = "74" ,              # variable id = 3, value id = 74
               "762" = "304092"          # variable id = 762, value id = 304092
               )

# Request data using the filter
ipc <- get_data_series_filter(operation = "IPC", filter = filter, periodicity = 1, 
                              unnest = TRUE, tip = "A", validate = FALSE)
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                                Nombre T3_Periodo Anyo Valor
#> 1 Alicante/Alacant. Índice general. Variación anual.        may. 2026   3.5
#> 2         Albacete. Índice general. Variación anual.        may. 2026   2.9
#> 3      Araba/Álava. Índice general. Variación anual.        may. 2026   3.3
```

- A variable can take a empty character `""` to get all its possible
  values (valid to filter data from tables as well).

``` r

# The filter is a list()
filter <- list("115" = "",      # variable id = 115, all values
               "3" = "83" ,     # variable id = 3, value id = 83
               "762" = "304092" # variable id = 762, value id = 304092
               )

# Request data using the filter
ipc <- get_data_series_filter(operation = "IPC", filter = filter, periodicity = 1, 
                              unnest = TRUE, tip = "A", validate = FALSE)
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                              Nombre T3_Periodo Anyo   Valor
#> 1               Salamanca. Índice general. Índice.        may. 2026 103.365
#> 2                  Huelva. Índice general. Índice.        may. 2026 102.264
#> 3             Ciudad Real. Índice general. Índice.        may. 2026 103.206
#> 4        Alicante/Alacant. Índice general. Índice.        may. 2026 102.957
#> 5                 Ourense. Índice general. Índice.        may. 2026 103.183
#> 6  Santa Cruz de Tenerife. Índice general. Índice.        may. 2026 102.861
#> 7               Coruña, A. Índice general. Índice.        may. 2026 103.135
#> 8              Valladolid. Índice general. Índice.        may. 2026 102.960
#> 9                  Huesca. Índice general. Índice.        may. 2026 103.037
#> 10                  Cádiz. Índice general. Índice.        may. 2026 102.767
#> 11                 Málaga. Índice general. Índice.        may. 2026 103.056
#> 12                 Teruel. Índice general. Índice.        may. 2026 103.197
#> 13               Albacete. Índice general. Índice.        may. 2026 102.674
#> 14              Rioja, La. Índice general. Índice.        may. 2026 102.698
#> 15             Pontevedra. Índice general. Índice.        may. 2026 103.482
#> 16                Melilla. Índice general. Índice.        may. 2026 102.886
#> 17                 Cuenca. Índice general. Índice.        may. 2026 103.252
#> 18                 Toledo. Índice general. Índice.        may. 2026 103.376
#> 19                Córdoba. Índice general. Índice.        may. 2026 102.893
#> 20                  Ceuta. Índice general. Índice.        may. 2026 102.576
#> 21                Granada. Índice general. Índice.        may. 2026 103.199
#> 22                  Ávila. Índice general. Índice.        may. 2026 102.891
#> 23              Tarragona. Índice general. Índice.        may. 2026 102.913
#> 24                  Soria. Índice general. Índice.        may. 2026 103.218
#> 25      Valencia/València. Índice general. Índice.        may. 2026 102.730
#> 26            Guadalajara. Índice general. Índice.        may. 2026 102.773
#> 27                 Burgos. Índice general. Índice.        may. 2026 102.752
#> 28                   Lugo. Índice general. Índice.        may. 2026 102.712
#> 29               Gipuzkoa. Índice general. Índice.        may. 2026 102.218
#> 30                Almería. Índice general. Índice.        may. 2026 102.942
#> 31                 Girona. Índice general. Índice.        may. 2026 103.312
#> 32                 Lleida. Índice general. Índice.        may. 2026 103.117
#> 33               Zaragoza. Índice general. Índice.        may. 2026 102.677
#> 34                 Zamora. Índice general. Índice.        may. 2026 102.992
#> 35     Castellón/Castelló. Índice general. Índice.        may. 2026 103.046
#> 36                   Jaén. Índice general. Índice.        may. 2026 102.641
#> 37               Asturias. Índice general. Índice.        may. 2026 102.401
#> 38            Araba/Álava. Índice general. Índice.        may. 2026 103.208
#> 39                Badajoz. Índice general. Índice.        may. 2026 102.756
#> 40                 Madrid. Índice general. Índice.        may. 2026 103.223
#> 41                Bizkaia. Índice general. Índice.        may. 2026 103.413
#> 42              Cantabria. Índice general. Índice.        may. 2026 103.109
#> 43            Palmas, Las. Índice general. Índice.        may. 2026 103.014
#> 44                Segovia. Índice general. Índice.        may. 2026 102.898
#> 45               Palencia. Índice general. Índice.        may. 2026 103.041
#> 46                Sevilla. Índice general. Índice.        may. 2026 102.806
#> 47                   León. Índice general. Índice.        may. 2026 103.271
#> 48         Balears, Illes. Índice general. Índice.        may. 2026 103.026
#> 49              Barcelona. Índice general. Índice.        may. 2026 102.762
#> 50                Navarra. Índice general. Índice.        may. 2026 102.897
#> 51                Cáceres. Índice general. Índice.        may. 2026 102.262
#> 52                 Murcia. Índice general. Índice.        may. 2026 102.819
```
