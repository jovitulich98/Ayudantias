Ayudantía 8
================

\#\#Instalar librerias

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.6.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.1     v dplyr   1.0.6
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 3.6.3

    ## Warning: package 'tibble' was built under R version 3.6.3

    ## Warning: package 'tidyr' was built under R version 3.6.3

    ## Warning: package 'readr' was built under R version 3.6.3

    ## Warning: package 'purrr' was built under R version 3.6.3

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## Warning: package 'forcats' was built under R version 3.6.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(cluster)
```

    ## Warning: package 'cluster' was built under R version 3.6.3

``` r
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 3.6.3

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(mclust)
```

    ## Warning: package 'mclust' was built under R version 3.6.3

    ## Package 'mclust' version 5.4.7
    ## Type 'citation("mclust")' for citing this R package in publications.

    ## 
    ## Attaching package: 'mclust'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

\#\#Se lee la base de datos

``` r
library(readr)
Spotify <- read_csv("Spotify.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   artist_name = col_character(),
    ##   track_name = col_character(),
    ##   album_name = col_character(),
    ##   danceability = col_double(),
    ##   energy = col_double(),
    ##   key = col_double(),
    ##   loudness = col_double(),
    ##   mode = col_double(),
    ##   speechiness = col_double(),
    ##   acousticness = col_double(),
    ##   instrumentalness = col_double(),
    ##   liveness = col_double(),
    ##   valence = col_double(),
    ##   tempo = col_double(),
    ##   duration_ms = col_double()
    ## )

## Omitir datos N/A

    ## [1] 13000    15

# Veificamos tipos de datos

``` r
str(Spotify)
```

    ## tibble [13,000 x 15] (S3: tbl_df/tbl/data.frame)
    ##  $ artist_name     : chr [1:13000] "Mother Nature Sound FX" "Johann Sebastian Bach" "Mother Nature Sound FX" "Frédéric Chopin" ...
    ##  $ track_name      : chr [1:13000] "Mountain Creeks" "Musikalisches Opfer, Op. 6, BWV 1079: Musical Offering, BWV 1079: Ricercar à 3" "Forest of Rainfall" "Waltz No.6 In D Flat, Op.64 No.1 -\"Minute\"" ...
    ##  $ album_name      : chr [1:13000] "! ! ! ! ! Earthbound ! ! ! ! !" "Bach: Variations & More" "! ! ! \" Scenes \" ! ! !" "Chopin Cosy Moments" ...
    ##  $ danceability    : num [1:13000] 0.222 0.287 0.235 0.322 0.447 0.117 0.596 0.445 0.363 0.159 ...
    ##  $ energy          : num [1:13000] 0.78 0.545 1 0.105 0.054 0.411 0.049 0.785 0.112 0.24 ...
    ##  $ key             : num [1:13000] 0 11 0 1 2 10 10 0 6 0 ...
    ##  $ loudness        : num [1:13000] -29.7 -16.2 -15.4 -25.7 -31.1 ...
    ##  $ mode            : num [1:13000] 1 0 0 1 1 0 1 1 1 0 ...
    ##  $ speechiness     : num [1:13000] 0.1 0.0309 0.0977 0.0401 0.0382 0.0654 0.0438 0.0353 0.0407 0.037 ...
    ##  $ acousticness    : num [1:13000] 0.67 0.706 0.79 0.99 0.974 0.000738 0.973 0.172 0.989 0.979 ...
    ##  $ instrumentalness: num [1:13000] 0.897 0.909 0.862 0.876 0.899 0.938 0.634 0 0.882 0.533 ...
    ##  $ liveness        : num [1:13000] 0.662 0.0482 0.96 0.142 0.137 0.886 0.074 0.372 0.102 0.501 ...
    ##  $ valence         : num [1:13000] 0.0127 0.84 0.00001 0.246 0.281 0.0463 0.343 0.554 0.0859 0.148 ...
    ##  $ tempo           : num [1:13000] 86.2 137 83.5 65.1 127.4 ...
    ##  $ duration_ms     : num [1:13000] 153469 362653 197488 118315 154706 ...

## Creamos un sampleo de datos

# De esta manera se demora menos en correr el codigo

``` r
set.seed(369)
sampleIndex <- sample(1:nrow(Spotify),8000, replace = F)
Spotify <- Spotify[sampleIndex,]
dim(Spotify)
```

    ## [1] 8000   15

## Separar datos

``` r
Spotify_char<- c("artist_name", "track_name", "album_name")
Spotify_num <- c("danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")


Spotifynum <- Spotify %>% 
  select(Spotify_num)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(Spotify_num)` instead of `Spotify_num` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

``` r
Spotifychar <- Spotify %>% 
  select(Spotify_char)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(Spotify_char)` instead of `Spotify_char` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

## Escalar datos numericos

``` r
spotifysca = scale(Spotifynum) %>% as_tibble()
```

## DBScan

# Despues de probar distintos parametros se escogieron los siguientes ya que la cantidad de puntos de ruido era relativamente aceptable y la cantidad de canciones en cada cluster no era tan grande, excepto en los cluster 1 y 2 que tienen la mayor cantidad de clusters

``` r
library(dbscan)
```

    ## Warning: package 'dbscan' was built under R version 3.6.3

``` r
set.seed(369)

model = dbscan(spotifysca, eps = 2, minPts = 8)

model
```

    ## DBSCAN clustering for 8000 objects.
    ## Parameters: eps = 2, minPts = 8
    ## The clustering contains 8 cluster(s) and 320 noise points.
    ## 
    ##    0    1    2    3    4    5    6    7    8 
    ##  320 5229 2368   18   13   20   10   13    9 
    ## 
    ## Available fields: cluster, eps, minPts

## Grafico Mode vs duration\_ms

# Podemos identificar cierta tendencia donde mientras mas alta la duración de la cancion o pertenece a un punto de ruido o pertenece al cluster 8. Tambien dadas las leves diferencias de color podemos identificar que en cuanto al mode igual se generaron clusters y se dividieron en cto eso.

``` r
ggplot(spotifysca, aes(duration_ms, mode, color = factor(model$cluster), size = duration_ms)) + 
  geom_point(alpha = 0.3) 
```

![](Ayudantia_8_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> \#\#
Fuzzy C-Means

``` r
library(e1071)
```

    ## Warning: package 'e1071' was built under R version 3.6.3

``` r
set.seed(369)

modelo_c_means <- cmeans(spotifysca,  7, m=2) 

modelo_c_means$membership %>% head()
```

    ##              1         2         3          4         5         6          7
    ## [1,] 0.1250583 0.1250654 0.1250594 0.18653636 0.1250678 0.1250596 0.18815316
    ## [2,] 0.1390693 0.1390709 0.1390695 0.15226635 0.1390715 0.1390696 0.15238279
    ## [3,] 0.1734212 0.1734166 0.1734205 0.06613470 0.1734152 0.1734203 0.06677149
    ## [4,] 0.1679139 0.1679123 0.1679137 0.07983223 0.1679118 0.1679136 0.08060258
    ## [5,] 0.1720424 0.1720361 0.1720415 0.06959134 0.1720341 0.1720412 0.07021333
    ## [6,] 0.1031306 0.1031342 0.1031312 0.24340696 0.1031353 0.1031313 0.24093051

## Plot duracion vs speechiness

# En este gráfico se puede notar mejor la tendencia de cada cluster pero aun asi no se ven grupos totalmente definidos

``` r
ggplot(spotifysca, aes(duration_ms, speechiness, color = factor(modelo_c_means$cluster), size = duration_ms)) + 
  geom_point(alpha = 0.3) 
```

![](Ayudantia_8_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> \#\# FPC
\# Indice bajo, por lo que existe alta variabilidad en los clusters. No
es lo ideal. Deberian tener una tendencia definida

``` r
matriz <- modelo_c_means$membership%*%t(modelo_c_means$membership) # producto matricial
(FPC <- sum(matriz*diag(nrow(matriz)))/nrow(matriz))
```

    ## [1] 0.1575119

``` r
library(mclust)

set.seed(369)

model_gmm = Mclust(spotifysca)

model_gmm 
```

    ## 'Mclust' model object: (EEV,5) 
    ## 
    ## Available components: 
    ##  [1] "call"           "data"           "modelName"      "n"             
    ##  [5] "d"              "G"              "BIC"            "loglik"        
    ##  [9] "df"             "bic"            "icl"            "hypvol"        
    ## [13] "parameters"     "z"              "classification" "uncertainty"

## Plot speechiness vs duration

``` r
ggplot(spotifysca) + 
  aes(x=duration_ms, y=speechiness, color=factor(model_gmm$classification)) + 
  geom_point(alpha=1)
```

![](Ayudantia_8_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot(model_gmm, what = "BIC")
```

![](Ayudantia_8_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
