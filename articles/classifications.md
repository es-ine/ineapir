# Classifications and hierarchy trees

## Classifications

There are variables whose values can change over time. An example is the
name of a municipality. For these cases, there is the concept of
classifications, which allows access to the values of a specific
classification. We can request the available classifications with the
[`get_metadata_classifications()`](https://inedifusion.github.io/ineapir/reference/get_metadata_classifications.md)
function.

``` r

library(ineapir)

# Get classifications
classifications <- get_metadata_classifications(lang = "EN")
head(classifications)
#>   Id          Nombre        Fecha
#> 1  1         CNAE 93 7.258428e+11
#> 2  2       CNAE 2009 1.230764e+12
#> 3  3         CNAE 74 1.262268e+11
#> 4  5 CPI System 1976 2.209212e+11
#> 5  6 CPI System 1983 4.417596e+11
#> 6  7 CPI System 1992 7.258428e+11
```

In the case of municipalities, there is a classification for each year,
so we can retrieve the municipalities for a particular year using the
[`get_metadata_values()`](https://inedifusion.github.io/ineapir/reference/get_metadata_values.md)
function and the `classification` argument.

``` r

# Select the classifications with name 'Geographical yyyy'
classifications <- get_metadata_classifications(lang = "EN")
head(classifications[grepl("geographical", classifications$Nombre,
                           ignore.case = TRUE),])
#>    Id            Nombre        Fecha
#> 24 29 Geographical 2007 1.199142e+12
#> 25 30 Geographical 2008 1.230764e+12
#> 26 31 Geographical 2009 1.262300e+12
#> 27 32 Geographical 2010 1.293836e+12
#> 28 33 Geographical 2011 1.325372e+12
#> 29 34 Geographical 2012 1.356995e+12

# Municipalities: id=19
# To retrieve the municipalities of 2007 we use the classificacion with id=29
municipalities <- get_metadata_values(variable = 19, classification = 29)
head(municipalities)
#>    Id Fk_Variable        Nombre Codigo FK_JerarquiaPadres
#> 1 456          19    Orbaitzeta  31195         32, 392378
#> 2 457          19        Orbara  31196         32, 392378
#> 3 458          19      Orísoain  31197         32, 392381
#> 4 459          19 Oronz/Orontze  31198         32, 392378
#> 5 460          19   Oroz-Betelu  31199         32, 392378
#> 6 461          19        Oteiza  31200         32, 392377
```

## Hierarchy trees

There are certain values that belong to a hierarchical structure and can
have parents and children. To obtain the children of a specific value,
we can use the
[`get_metadata_values()`](https://inedifusion.github.io/ineapir/reference/get_metadata_values.md)
function with the `variable` and `value` arguments. For example, if we
want to find the provinces of Galicia:

``` r

# Variable: Autonomous communities (id=70)
# Value: Galicia (id=9008)
# Get the children of id=9008 (provinces of Galicia)
provinces <- get_metadata_values(variable = 70, value = 9008)
provinces
#>   Id Fk_Variable     Nombre Codigo FK_JerarquiaPadres
#> 1 16         115  Coruña, A     15               9008
#> 2 28         115       Lugo     27               9008
#> 3 36         115 Pontevedra     36               9008
#> 4 53         115    Ourense     32               9008
```

If we want to go deeper into the hierarchical structure we can use the
`hierarchy` argument, which represents the depth.

``` r

# Variable: Autonomous communities (id=70)
# Value: Galicia (id=9008)
# Get the children of each province (municipalities of Galicia)
municipalities <- get_metadata_values(variable = 70, value = 9008, hierarchy = 1)
head(municipalities)
#>   Id_0 Fk_Variable_0  Nombre_0 Codigo_0 FK_JerarquiaPadres_0 Id_1 Fk_Variable_1
#> 1   16           115 Coruña, A       15                 9008 3403            19
#> 2   16           115 Coruña, A       15                 9008 4508            19
#> 3   16           115 Coruña, A       15                 9008 4509            19
#> 4   16           115 Coruña, A       15                 9008 4510            19
#> 5   16           115 Coruña, A       15                 9008 4511            19
#> 6   16           115 Coruña, A       15                 9008 4512            19
#>   Nombre_1 Codigo_1 FK_JerarquiaPadres_1
#> 1  Sobrado    15080           16, 392352
#> 2     Ares    15004           16, 392350
#> 3  Arteixo    15005           16, 392350
#> 4    Arzúa    15006           16, 392352
#> 5  Baña, A    15007           16, 392351
#> 6 Bergondo    15008           16, 392350
```

If we want the root of the tree to be the variable, we do not specify
any value.

``` r

# Variable: Autonomous communities (id=70)
# Get the children of each Autonomous communities (provinces)
provinces <- get_metadata_values(variable = 70, hierarchy = 1)
head(provinces)
#>   Id_0 Fk_Variable_0  Nombre_0 Codigo_0          FK_JerarquiaPadres_0 Id_1
#> 1 8995            70   Melilla       19         16473, 274511, 274508   52
#> 2 8997            70 Andalucía       01 16473, 274511, 457344, 274508    5
#> 3 8997            70 Andalucía       01 16473, 274511, 457344, 274508   12
#> 4 8997            70 Andalucía       01 16473, 274511, 457344, 274508   15
#> 5 8997            70 Andalucía       01 16473, 274511, 457344, 274508   19
#> 6 8997            70 Andalucía       01 16473, 274511, 457344, 274508   22
#>   Fk_Variable_1 Nombre_1 Codigo_1 FK_JerarquiaPadres_1
#> 1           115  Melilla       52                 8995
#> 2           115  Almería       04                 8997
#> 3           115    Cádiz       11                 8997
#> 4           115  Córdoba       14                 8997
#> 5           115  Granada       18                 8997
#> 6           115   Huelva       21                 8997
```

Additionally, we can filter out the variables and values that only
interest us with the `filter` argument.

1.  Example 1.

``` r

# We define the filter as a list of variables and values
filter <- list("70" = 9008 # variable id = 70, value id = 9008 (Galicia)
               )
# Get the children of id=9008 (provinces of Galicia)
provinces <- get_metadata_values(variable = 70, filter = filter, hierarchy = 1,
                                 validate = FALSE)
head(provinces)
#>   Id_0 Fk_Variable_0 Nombre_0 Codigo_0          FK_JerarquiaPadres_0 Id_1
#> 1 9008            70  Galicia       12 16473, 274511, 457344, 274508   16
#> 2 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 3 9008            70  Galicia       12 16473, 274511, 457344, 274508   36
#> 4 9008            70  Galicia       12 16473, 274511, 457344, 274508   53
#>   Fk_Variable_1   Nombre_1 Codigo_1 FK_JerarquiaPadres_1
#> 1           115  Coruña, A       15                 9008
#> 2           115       Lugo       27                 9008
#> 3           115 Pontevedra       36                 9008
#> 4           115    Ourense       32                 9008
```

2.  Example 2.

``` r

# We define the filter as a list of variables and values
filter <- list("115" =  "" # variable id = 115, all values
               )
# Get the children of id=70 (provinces of Spain)
provinces <- get_metadata_values(variable = 70, filter = filter, hierarchy = 1,
                                 validate = FALSE)
head(provinces)
#>   Id_0 Fk_Variable_0  Nombre_0 Codigo_0          FK_JerarquiaPadres_0 Id_1
#> 1 8995            70   Melilla       19         16473, 274511, 274508   52
#> 2 8997            70 Andalucía       01 16473, 274511, 457344, 274508    5
#> 3 8997            70 Andalucía       01 16473, 274511, 457344, 274508   12
#> 4 8997            70 Andalucía       01 16473, 274511, 457344, 274508   15
#> 5 8997            70 Andalucía       01 16473, 274511, 457344, 274508   19
#> 6 8997            70 Andalucía       01 16473, 274511, 457344, 274508   22
#>   Fk_Variable_1 Nombre_1 Codigo_1 FK_JerarquiaPadres_1
#> 1           115  Melilla       52                 8995
#> 2           115  Almería       04                 8997
#> 3           115    Cádiz       11                 8997
#> 4           115  Córdoba       14                 8997
#> 5           115  Granada       18                 8997
#> 6           115   Huelva       21                 8997
```

3.  Example 3.

``` r

# We define the filter as a list of variables and values
filter <- list("70" = 9008, # variable id = 70, value id = 9008 (Galicia) 
               "115" = 28 # variable id = 115, value id = 28 (Lugo)
               )
# Get the children of id=28 (municipalities of Lugo province)
municipalities <- get_metadata_values(variable = 70, filter = filter, 
                                      hierarchy = 2, validate = FALSE)
head(municipalities)
#>   Id_0 Fk_Variable_0 Nombre_0 Codigo_0          FK_JerarquiaPadres_0 Id_1
#> 1 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 2 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 3 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 4 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 5 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 6 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#>   Fk_Variable_1 Nombre_1 Codigo_1 FK_JerarquiaPadres_1 Id_2 Fk_Variable_2
#> 1           115     Lugo       27                 9008  570            19
#> 2           115     Lugo       27                 9008 2780            19
#> 3           115     Lugo       27                 9008 2781            19
#> 4           115     Lugo       27                 9008 2967            19
#> 5           115     Lugo       27                 9008 2968            19
#> 6           115     Lugo       27                 9008 2969            19
#>        Nombre_2 Codigo_2 FK_JerarquiaPadres_2
#> 1          Lugo    27028           28, 392355
#> 2       Vilalba    27065           28, 392353
#> 3       Baralla    27901           28, 392356
#> 4        Abadín    27001           28, 392353
#> 5         Alfoz    27002           28, 392354
#> 6 Antas de Ulla    27003           28, 392355
```

4.  Example 4.

``` r

# We define the filter as a list of variables and values
filter <- list("70" = 9008, # variable id = 70, value id = 9008 (Galicia)  
               "115" =  28 , # variable id = 115, value id = 28 (Lugo)
               "19" = 570 # variable id = 19, value id = 570 (Lugo)
               )
# Get the children of id=570 (census sections of Lugo municipality)
sections <- get_metadata_values(variable = 70, filter = filter, hierarchy = 4,
                                validate = FALSE)
head(sections)
#>   Id_0 Fk_Variable_0 Nombre_0 Codigo_0          FK_JerarquiaPadres_0 Id_1
#> 1 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 2 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 3 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 4 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 5 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#> 6 9008            70  Galicia       12 16473, 274511, 457344, 274508   28
#>   Fk_Variable_1 Nombre_1 Codigo_1 FK_JerarquiaPadres_1 Id_2 Fk_Variable_2
#> 1           115     Lugo       27                 9008  570            19
#> 2           115     Lugo       27                 9008  570            19
#> 3           115     Lugo       27                 9008  570            19
#> 4           115     Lugo       27                 9008  570            19
#> 5           115     Lugo       27                 9008  570            19
#> 6           115     Lugo       27                 9008  570            19
#>   Nombre_2 Codigo_2 FK_JerarquiaPadres_2   Id_3 Fk_Variable_3         Nombre_3
#> 1     Lugo    27028           28, 392355 344156           846 Lugo distrito 01
#> 2     Lugo    27028           28, 392355 344156           846 Lugo distrito 01
#> 3     Lugo    27028           28, 392355 344156           846 Lugo distrito 01
#> 4     Lugo    27028           28, 392355 344160           846 Lugo distrito 02
#> 5     Lugo    27028           28, 392355 344160           846 Lugo distrito 02
#> 6     Lugo    27028           28, 392355 344160           846 Lugo distrito 02
#>   Codigo_3 FK_JerarquiaPadres_3   Id_4 Fk_Variable_4           Nombre_4
#> 1  2702801                  570 344157           847 Lugo sección 01001
#> 2  2702801                  570 344158           847 Lugo sección 01002
#> 3  2702801                  570 344159           847 Lugo sección 01003
#> 4  2702802                  570 344161           847 Lugo sección 02001
#> 5  2702802                  570 344162           847 Lugo sección 02002
#> 6  2702802                  570 344163           847 Lugo sección 02003
#>     Codigo_4 FK_JerarquiaPadres_4
#> 1 2702801001               344156
#> 2 2702801002               344156
#> 3 2702801003               344156
#> 4 2702802001               344160
#> 5 2702802002               344160
#> 6 2702802003               344160
```
