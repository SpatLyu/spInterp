anusplin
================

<!-- ---
title: "anusplin"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{anusplin}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
--- -->

``` r
devtools::load_all()
#> ℹ Loading spInterp
#> Warning: package 'testthat' was built under R version 4.2.3
library(spInterp)
library(data.table)
#> 
#> Attaching package: 'data.table'
#> 
#> The following object is masked from 'package:spInterp':
#> 
#>     last
# library(ggplot2)
# library(Ipaper)
```

``` r
dat <- dat_RH[, .(lon, lat, alt, RH)]
X <- dat[, .(lon, lat, alt)] %>% as.matrix()
Y <- dat[, .(RH)] %>% as.matrix()

f_alt <- system.file("anusplin_demo/china_dem_025deg.txt", package = "spInterp")
range <- c(69.625, 140.375, 14.625, 55.375)
# range <- c(70, 140, 15, 55) # 高程数据的range有误
```

# 1. anusplin

## 1.1. fast example

``` r
outdir = "output"

anusplin_make_param(dat, "RH", range, file.alt = f_alt) -> param
anusplin_write_setting(param, outdir, is.run = TRUE, overwrite = TRUE)
#> [info] setting anusplin path.
anusplin_read_output(param$lapgrd, outdir) -> res

res
#>             x     y      RH
#>         <num> <num>   <num>
#>     1:  69.75 55.25 156.947
#>     2:  70.00 55.25 155.612
#>     3:  70.25 55.25 154.272
#>     4:  70.50 55.25 152.945
#>     5:  70.75 55.25 151.638
#>    ---                     
#> 45962: 139.00 14.75 218.933
#> 45963: 139.25 14.75 221.611
#> 45964: 139.50 14.75 224.317
#> 45965: 139.75 14.75 227.053
#> 45966: 140.00 14.75 229.818
```

## 1.2. 另外一种等价的调用方法

``` r
spInterp_anusplin(X, Y, range = range, file.alt = f_alt)
#> [ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#> List of 2
#>  $ coord    :Classes 'data.table' and 'data.frame':  45966 obs. of  2 variables:
#>   ..$ x: num [1:45966] 69.8 70 70.2 70.5 70.8 ...
#>   ..$ y: num [1:45966] 55.2 55.2 55.2 55.2 55.2 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ predicted:Classes 'data.table' and 'data.frame':  45966 obs. of  1 variable:
#>   ..$ RH: num [1:45966] 157 156 154 153 152 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "class")= chr "spInterp"
```

## 1.3. k-fold cross validation

``` r
r = kford_ml(X, Y, FUN = spInterp_anusplin, 
  file.alt = f_alt, range = range, kfold = 5)
#>   |                                                                              |                                                                      |   0%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |==============                                                        |  20%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |============================                                          |  40%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |==========================================                            |  60%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |========================================================              |  80%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |======================================================================| 100%
r
#> # A tibble: 6 × 12
#>   kfold     R pvalue    R2   NSE   KGE  RMSE   MAE   Bias Bias_perc    AI n_sim
#>   <chr> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>     <dbl> <dbl> <int>
#> 1 Fold1 0.877      0 0.769 0.756 0.876  5.94  4.20  0.192     0.003 0.936   168
#> 2 Fold2 0.92       0 0.846 0.833 0.918  5.49  4.14 -1.15     -0.017 0.957   168
#> 3 Fold3 0.929      0 0.863 0.86  0.924  4.84  3.52 -0.399    -0.006 0.963   168
#> 4 Fold4 0.928      0 0.861 0.859 0.887  4.61  3.47 -0.512    -0.007 0.96    168
#> 5 Fold5 0.913      0 0.833 0.827 0.911  5.22  3.85  0.284     0.004 0.955   168
#> 6 all   0.913      0 0.834 0.83  0.909  5.24  3.84 -0.318    -0.005 0.955   840
#> 
#> Fold index:
#> List of 5
#>  $ Fold1: int [1:168] 1 2 23 32 55 56 59 67 72 75 ...
#>  $ Fold2: int [1:168] 4 5 17 22 25 26 29 30 34 38 ...
#>  $ Fold3: int [1:168] 3 6 9 19 21 28 33 35 36 37 ...
#>  $ Fold4: int [1:168] 8 10 11 12 15 16 20 31 42 45 ...
#>  $ Fold5: int [1:168] 7 13 14 18 24 27 44 49 52 53 ...
#> NULL
```

# 2. 不考虑高程时的结果

``` r
spInterp_anusplin(X, Y, range = range, file.alt = f_alt)
#> [ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#> List of 2
#>  $ coord    :Classes 'data.table' and 'data.frame':  45966 obs. of  2 variables:
#>   ..$ x: num [1:45966] 69.8 70 70.2 70.5 70.8 ...
#>   ..$ y: num [1:45966] 55.2 55.2 55.2 55.2 55.2 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ predicted:Classes 'data.table' and 'data.frame':  45966 obs. of  1 variable:
#>   ..$ RH: num [1:45966] 157 156 154 153 152 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "class")= chr "spInterp"
```

## 2.1. k-fold cross validation

``` r
X = dat[, .(lon, lat, alt)] %>% as.matrix()
Y = dat[, .(RH)] %>% as.matrix()

r = kford_ml(X, Y, FUN = spInterp_anusplin, 
  file.alt = NULL, range = range, kfold = 5)
#>   |                                                                              |                                                                      |   0%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |==============                                                        |  20%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |============================                                          |  40%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |==========================================                            |  60%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |========================================================              |  80%[ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#>   |                                                                              |======================================================================| 100%
r
#> # A tibble: 6 × 12
#>   kfold     R pvalue    R2   NSE   KGE  RMSE   MAE   Bias Bias_perc    AI n_sim
#>   <chr> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>     <dbl> <dbl> <int>
#> 1 Fold1 0.872      0 0.76  0.748 0.869  6.03  4.15  0.446     0.007 0.933   168
#> 2 Fold2 0.918      0 0.843 0.839 0.91   5.38  4.01 -0.609    -0.009 0.957   168
#> 3 Fold3 0.926      0 0.858 0.856 0.917  4.91  3.55 -0.085    -0.001 0.962   168
#> 4 Fold4 0.927      0 0.859 0.858 0.872  4.63  3.44 -0.259    -0.004 0.959   168
#> 5 Fold5 0.915      0 0.837 0.831 0.907  5.16  3.91  0.731     0.011 0.955   168
#> 6 all   0.912      0 0.831 0.829 0.899  5.24  3.81  0.045     0.001 0.954   840
#> 
#> Fold index:
#> List of 5
#>  $ Fold1: int [1:168] 1 2 23 32 55 56 59 67 72 75 ...
#>  $ Fold2: int [1:168] 4 5 17 22 25 26 29 30 34 38 ...
#>  $ Fold3: int [1:168] 3 6 9 19 21 28 33 35 36 37 ...
#>  $ Fold4: int [1:168] 8 10 11 12 15 16 20 31 42 45 ...
#>  $ Fold5: int [1:168] 7 13 14 18 24 27 44 49 52 53 ...
#> NULL
```

# 3. 与adw进行对比

``` r
r <- kford_ml(X[, c("lon", "lat")], Y,
  FUN = spInterp_adw,
  range = range, res = 0.5, cdd = 450
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==============                                                        |  20%  |                                                                              |============================                                          |  40%  |                                                                              |==========================================                            |  60%  |                                                                              |========================================================              |  80%  |                                                                              |======================================================================| 100%
r
#> # A tibble: 6 × 12
#>   kfold     R pvalue    R2   NSE   KGE  RMSE   MAE   Bias Bias_perc    AI n_sim
#>   <chr> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>     <dbl> <dbl> <int>
#> 1 Fold1 0.873      0 0.762 0.756 0.859  5.94  4.14  0.351     0.005 0.932   168
#> 2 Fold2 0.916      0 0.839 0.837 0.886  5.41  3.96 -0.438    -0.007 0.954   168
#> 3 Fold3 0.93       0 0.864 0.863 0.876  4.69  3.55  0.198     0.003 0.961   166
#> 4 Fold4 0.932      0 0.869 0.861 0.85   4.57  3.44 -0.682    -0.01  0.959   168
#> 5 Fold5 0.915      0 0.836 0.833 0.877  5.13  3.82  0.783     0.012 0.952   168
#> 6 all   0.912      0 0.833 0.832 0.872  5.17  3.78  0.042     0.001 0.952   838
#> 
#> Fold index:
#> List of 5
#>  $ Fold1: int [1:168] 1 2 23 32 55 56 59 67 72 75 ...
#>  $ Fold2: int [1:168] 4 5 17 22 25 26 29 30 34 38 ...
#>  $ Fold3: int [1:168] 3 6 9 19 21 28 33 35 36 37 ...
#>  $ Fold4: int [1:168] 8 10 11 12 15 16 20 31 42 45 ...
#>  $ Fold5: int [1:168] 7 13 14 18 24 27 44 49 52 53 ...
#> NULL
```

# TODO

- [ ] 高程自动采样
