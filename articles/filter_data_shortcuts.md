# How to filter data using shortcut names

Instead of using numeric codes, it is possible to use certain shortcut
names when the `filter` argument is passed. However, for a better
performance is recommended to use numeric codes. The available shortcuts
for variables are shown below.

| Shortcut        | Variable ID        | Comment                |
|-----------------|--------------------|------------------------|
| nac             | 349                | National               |
| ccaa            | 70                 | Autonomous Communities |
| prov            | 115                | Provinces              |
| mun             | 19                 | Municipalities         |
| island          | 20                 | Islands                |
| isla            | 20                 | Islands                |
| group           | 762                | CPI groups             |
| grupo           | 762                | CPI groups             |
| subgroup        | 763                | CPI subgroups          |
| subgrupo        | 763                | CPI subgroups          |
| class           | 764                | CPI class              |
| clase           | 764                | CPI class              |
| subclass        | 765                | CPI subclass           |
| subclase        | 765                | CPI subclass           |
| heading         | 270                | CPI headings           |
| rubrica         | 270                | CPI headings           |
| specialgroup    | 269                | CPI special groups     |
| grupoespecial   | 269                | CPI special groups     |
| datatype        | 3                  | Type of data           |
| tipodato        | 3                  | Type of data           |
| sex             | 18                 | Sex                    |
| sexo            | 18                 | Sex                    |
| age1            | 355                | Simple age             |
| edad1           | 355                | Simple age             |
| aget            | 356                | Age totals             |
| edadt           | 356                | Age totals             |
| ageg            | 360                | Age groups             |
| edadg           | 360                | Age groups             |
| ages            | 357                | Age semi-intervals     |
| edads           | 357                | Age semi-intervals     |
| age             | 355, 356, 357, 360 | Age wrapper            |
| edad            | 355, 356, 357, 360 | Age wrapper            |
| generation      | 612                | Generation/ages        |
| generacion      | 612                | Generation/ages        |
| nationality     | 141                | Nationality            |
| nacionalidad    | 141                | Nationality            |
| birthcountry    | 431, 432           | Country of birth       |
| paisnacimiento  | 431, 432           | Country of birth       |
| birthplace      | 97                 | Place of birth         |
| lugarnacimiento | 97                 | Place of birth         |
| effectscorr     | 544                | Correction of effects  |
| efectoscorr     | 544                | Correction of effects  |
| values          | \-                 | Values wrapper         |

## Filtering data from tables

To use a filter with shortcuts it is necessary to pass the argument
`filter` using a shortcut as variable and a name as value.

### Example one

We are going to filter the table [Índices nacionales: general y de
grupos ECOICOP](https://www.ine.es/jaxiT3/Tabla.htm?t=76125) (Id 76125):

``` r

library(ineapir)

# Filter using shortcuts (CPI)
filter <- list(tipodato = "variación anual" , # variable id = 3
               grupo = "índice general"       # variable id = 762
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

Besides, we can also use the *values* wrapper.

``` r

# Filter with the values wrapper
filter <- list(values = c("variación anual" , "índice general"))

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

If we add a minus sign to a value, the value will be excluded from the
filter.

``` r

# Filter with the values wrapper
filter <- list(values = c("variación anual" , "-índice general"))

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

### Example two

We are going to filter the table [PIB pm Oferta (Indices de volumen
encadenado)](https://www.ine.es/jaxiT3/Tabla.htm?t=67822) (Id 67822):

``` r

# Filter with the values wrapper (GDP)
filter <- list(values = c("producto interior", "datos ajustados", "variación anual"))

# Request data using the filter and shortcut = TRUE
pib <- get_data_table(idTable = 67822, filter = filter, unnest = TRUE,
                      tip = "A", nlast = 5, validate = FALSE)
head(pib[,c("Nombre", "T3_Periodo", "Anyo", "Valor")])
#>                                                                                                                                                              Nombre
#> 1   Total Nacional. Datos ajustados de estacionalidad y calendario. Producto interior bruto a precios de mercado. Variación anual. Índices de volumen encadenados. 
#> 1.1 Total Nacional. Datos ajustados de estacionalidad y calendario. Producto interior bruto a precios de mercado. Variación anual. Índices de volumen encadenados. 
#> 1.2 Total Nacional. Datos ajustados de estacionalidad y calendario. Producto interior bruto a precios de mercado. Variación anual. Índices de volumen encadenados. 
#> 1.3 Total Nacional. Datos ajustados de estacionalidad y calendario. Producto interior bruto a precios de mercado. Variación anual. Índices de volumen encadenados. 
#> 1.4 Total Nacional. Datos ajustados de estacionalidad y calendario. Producto interior bruto a precios de mercado. Variación anual. Índices de volumen encadenados. 
#>     T3_Periodo Anyo  Valor
#> 1           T1 2026 2.7126
#> 1.1         T4 2025 2.6461
#> 1.2         T3 2025 2.7030
#> 1.3         T2 2025 2.8792
#> 1.4         T1 2025 3.0776
```

### Example three

We are going to filter the table [Población residente por fecha, sexo y
edad (desde 1971)](https://www.ine.es/jaxiT3/Tabla.htm?t=56934) (Id
56934)

``` r

# Filter with the values wrapper (population)
filter <- list(values = c("todas las edades", "total"))

# Request data using the filter
pob <- get_data_table(idTable = 56934, filter = filter, unnest = TRUE, 
                      tip = "A", nlast = 5, validate = FALSE)
pob[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                                           Nombre
#> 1   Total Nacional. Todas las edades. Total. Población. Número. 
#> 1.1 Total Nacional. Todas las edades. Total. Población. Número. 
#> 1.2 Total Nacional. Todas las edades. Total. Población. Número. 
#> 1.3 Total Nacional. Todas las edades. Total. Población. Número. 
#> 1.4 Total Nacional. Todas las edades. Total. Población. Número. 
#>          T3_Periodo Anyo    Valor
#> 1     1 de enero de 2025 49128297
#> 1.1 1 de octubre de 2024 48999880
#> 1.2   1 de julio de 2024 48821936
#> 1.3   1 de abril de 2024 48701130
#> 1.4   1 de enero de 2024 48619695
```

## Filtering data from series

It is necessary to pass the argument `filter` of the
[`get_data_series_filter()`](https://inedifusion.github.io/ineapir/reference/get_data_series_filter.md)
function. We use a shortcut as variable and a name as value.

### Example one

``` r

# Filter with the values wrapper (CPI)
filter <- list(values = c("variación anual" , "índice general", "total nacional"))

# Request data using the filter
ipc <- get_data_series_filter(operation = "IPC", filter = filter,
                              periodicity = 1, unnest = TRUE, tip = "A",
                              validate = FALSE)
ipc[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                                              Nombre T3_Periodo
#> 1                       Nacional. Índice general. Variación anual.        jun.
#> 2 Nacional. Índice general. Variación anual. Impuestos constantes.        may.
#>   Anyo Valor
#> 1 2026   3.2
#> 2 2026   4.4
```

### Example two

``` r

# Filter with the values wrapper (GDP)
filter <- list(values = c("producto interior", "datos ajustados",
                       "volumen encadenado", "variación anual"))

# Request data using the filter
pib <- get_data_series_filter(operation = "CNTR2010", filter = filter,
                              periodicity = 3, unnest = TRUE, tip = "A",
                              validate = FALSE)
pib[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                                                                                                                                                       Nombre
#> 1            Total Nacional. Datos ajustados de estacionalidad y calendario. Producto interior bruto a precios de mercado. Variación anual. Índices de volumen encadenados. 
#> 2 Total Nacional. Base 2010. Datos ajustados de estacionalidad y calendario. Producto interior bruto a precios de mercado. Variación anual. Índices de volumen encadenados. 
#>   T3_Periodo Anyo  Valor
#> 1         T1 2026 2.7126
#> 2         T2 2019 2.2513
```

### Example three

``` r

# Filter with the values wrapper combine with individual shortcuts (population)
filter <- list(values = c("todas las edades", "total nacional"), nacionalidad = "total",
               sexo = "total", paisnacimiento = "total" )

# Request data using the filter
pob <- get_data_series_filter(operation = "ECP", filter = filter,
                              periodicity = 3, unnest = TRUE, tip = "A",
                              validate = FALSE)
pob[,c("Nombre", "T3_Periodo", "Anyo", "Valor")]
#>                                                                       Nombre
#> 1 Total Nacional. Total. Todas las edades. Total. Total. Población. Número. 
#> 2 Total Nacional. Total. Todas las edades. Total. Total. Población. Número. 
#>   T3_Periodo Anyo    Valor
#> 1         T1 2025 49128297
#> 2         T1 2025 49128297
```
