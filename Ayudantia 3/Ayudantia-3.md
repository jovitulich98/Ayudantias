Ayudantia 3: Outliers
================

## Actividad Ayudantia 3

Replicar el analisis de outliers, debes elegir uno de los dos csv
disponibles (pokemon o titanic) y realizar el analisis con algunas de
las variables numericas y realizar un pequeño analisis en relacion a los
datos encontrados como outliers (en caso de que eligas el csv del
titanic solo debes evaluar las columnas AGE y FNLWGT)

## Outliers

Caso 1:

``` r
library(datasets)
library(readr)
titanic <- read_csv("C:/Users/josev/Desktop/Ayudantias/Ayudantia 3/titanic.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   AGE = col_double(),
    ##   WORKCLASS = col_character(),
    ##   FNLWGT = col_double(),
    ##   EDUCATION = col_character(),
    ##   EDUCATION.NUM = col_double(),
    ##   MARITAL.STATUS = col_character(),
    ##   OCCUPATION = col_character(),
    ##   RELATIONSHIP = col_character(),
    ##   RACE = col_character(),
    ##   SEX = col_character(),
    ##   CAPITAL.GAIN = col_double(),
    ##   CAPITAL.LOSS = col_double(),
    ##   HOURS.PER.WEEK = col_double(),
    ##   NATIVE.COUNTRY = col_character(),
    ##   INCOME = col_character()
    ## )

``` r
attach(titanic)

age = boxplot(AGE, horizontal = TRUE)
```

![](Ayudantia-3_files/figure-gfm/cargar%20y%20revisar%20datos-1.png)<!-- -->

``` r
stats_age = boxplot.stats(AGE)

age
```

    ## $stats
    ##      [,1]
    ## [1,]   17
    ## [2,]   28
    ## [3,]   37
    ## [4,]   48
    ## [5,]   78
    ## 
    ## $n
    ## [1] 29286
    ## 
    ## $conf
    ##          [,1]
    ## [1,] 36.81535
    ## [2,] 37.18465
    ## 
    ## $out
    ##   [1] 90 90 90 82 80 84 90 90 90 90 83 80 84 90 84 81 83 90 82 80 82 81 79 84 84
    ##  [26] 84 80 81 79 90 80 85 81 82 90 80 79 79 81 90 81 83 79 80 79 79 90 90 81 82
    ##  [51] 80 84 87 90 82 82 88 80 90 79 90 90 90 90 80 90 80 80 90 81 90 90 80 90 81
    ##  [76] 79 81 79 82 90 90 81 80 86 82 90 79 90 82 80 90 90 90 79 90 88 90 79 80 79
    ## [101] 90 84 80 85 83 81 79 90 80 90 80 84 90 80 82 81 79 82 83 85 90 79 80 79 79
    ## [126] 90
    ## 
    ## $group
    ##   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [112] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## 
    ## $names
    ## [1] "1"

``` r
stats_age
```

    ## $stats
    ## [1] 17 28 37 48 78
    ## 
    ## $n
    ## [1] 29286
    ## 
    ## $conf
    ## [1] 36.81535 37.18465
    ## 
    ## $out
    ##   [1] 90 90 90 82 80 84 90 90 90 90 83 80 84 90 84 81 83 90 82 80 82 81 79 84 84
    ##  [26] 84 80 81 79 90 80 85 81 82 90 80 79 79 81 90 81 83 79 80 79 79 90 90 81 82
    ##  [51] 80 84 87 90 82 82 88 80 90 79 90 90 90 90 80 90 80 80 90 81 90 90 80 90 81
    ##  [76] 79 81 79 82 90 90 81 80 86 82 90 79 90 82 80 90 90 90 79 90 88 90 79 80 79
    ## [101] 90 84 80 85 83 81 79 90 80 90 80 84 90 80 82 81 79 82 83 85 90 79 80 79 79
    ## [126] 90

``` r
titanic1 <- AGE[AGE < 80]
length(AGE) - length(titanic1)
```

    ## [1] 107

``` r
boxplot(titanic1, horizontal = TRUE)
```

![](Ayudantia-3_files/figure-gfm/primera%20limpieza-1.png)<!-- -->

``` r
boxplot.stats(titanic1)
```

    ## $stats
    ## [1] 17 28 37 47 75
    ## 
    ## $n
    ## [1] 29179
    ## 
    ## $conf
    ## [1] 36.82426 37.17574
    ## 
    ## $out
    ##   [1] 78 78 76 78 76 76 76 76 77 77 77 78 76 79 76 77 79 77 76 78 77 78 79 77 77
    ##  [26] 79 76 76 77 79 76 76 76 76 76 77 79 76 78 76 79 76 77 76 76 79 76 77 78 76
    ##  [51] 77 77 77 76 78 76 76 76 77 76 79 79 76 77 78 78 77 76 79 78 77 76 77 78 79
    ##  [76] 78 77 79 79 77 77 77 76 76 76 76 76 77 76 76 79 78 76 78 76 76 77 79 76 78
    ## [101] 76 78 78 79 76 79 79

``` r
titanic1 <- AGE[AGE < 76]
length(AGE) - length(titanic1)
```

    ## [1] 214

``` r
boxplot(titanic1, horizontal = TRUE)
```

![](Ayudantia-3_files/figure-gfm/segunda%20limpieza-1.png)<!-- -->

``` r
boxplot.stats(titanic1)
```

    ## $stats
    ## [1] 17 28 37 47 75
    ## 
    ## $n
    ## [1] 29072
    ## 
    ## $conf
    ## [1] 36.82393 37.17607
    ## 
    ## $out
    ## numeric(0)
