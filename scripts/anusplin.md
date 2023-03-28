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

res <- 1
range <- c(70, 140, 15, 55)
r_dem <- get_chinadem(res = res) %>% round(1)
f_dem <- "output/dem.asc"

write_dem(r_dem, f_dem, digits = 1)
```

# 1. anusplin

## 1.1. fast example

``` r
outdir <- "output"

anusplin_make_param(dat, "RH", range, res, file.alt = f_dem, cvt.coef = 1e3) -> param
anusplin_write_setting(param, outdir, is.run = TRUE, overwrite = TRUE)
#> [info] setting anusplin path.
anusplin_read_output(param$lapgrd, outdir) -> ans

ans
#>           x     y      RH
#>       <num> <num>   <num>
#>    1:  70.5  54.5 144.115
#>    2:  71.5  54.5 139.115
#>    3:  72.5  54.5 134.381
#>    4:  73.5  54.5 130.029
#>    5:  74.5  54.5 126.068
#>   ---                    
#> 2304: 111.5  15.5  77.389
#> 2305: 119.5  15.5  77.775
#> 2306: 120.5  15.5  80.525
#> 2307: 121.5  15.5  84.006
#> 2308: 122.5  15.5  89.231
```

## 1.2. 另外一种等价的调用方法

``` r
spInterp_anusplin(X, Y, range = range, res = res, file.alt = f_dem)
#> [ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#> List of 2
#>  $ coord    :Classes 'data.table' and 'data.frame':  2308 obs. of  2 variables:
#>   ..$ x: num [1:2308] 70.5 71.5 72.5 73.5 74.5 75.5 76.5 77.5 78.5 79.5 ...
#>   ..$ y: num [1:2308] 54.5 54.5 54.5 54.5 54.5 54.5 54.5 54.5 54.5 54.5 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ predicted:Classes 'data.table' and 'data.frame':  2308 obs. of  1 variable:
#>   ..$ RH: num [1:2308] 144 139 134 130 126 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "class")= chr "spInterp"
#> 
#> [data] ----------------
#> class       : SpatRaster 
#> dimensions  : 40, 70, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 70, 140, 15, 55  (xmin, xmax, ymin, ymax)
#> coord. ref. :  
#> source(s)   : memory
#> name        :      RH 
#> min value   :  11.127 
#> max value   : 408.459
```

## 1.3. k-fold cross validation

``` r
r = kford_ml(X, Y, FUN = spInterp_anusplin, 
  range = range, res = res, file.alt = f_dem, kfold = 5)
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
#> 1 Fold1 0.872      0 0.761 0.748 0.872  6.04  4.30  0.188     0.003 0.933   168
#> 2 Fold2 0.91       0 0.828 0.814 0.908  5.79  4.32 -1.18     -0.017 0.951   168
#> 3 Fold3 0.922      0 0.85  0.847 0.917  5.07  3.65 -0.348    -0.005 0.96    168
#> 4 Fold4 0.925      0 0.855 0.851 0.885  4.74  3.58 -0.755    -0.011 0.958   168
#> 5 Fold5 0.917      0 0.841 0.836 0.914  5.08  3.71  0.283     0.004 0.957   168
#> 6 all   0.909      0 0.826 0.821 0.903  5.37  3.91 -0.361    -0.005 0.952   840
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
spInterp_anusplin(X, Y, range, res, file.alt = f_dem)
#> [ok] anusplin_path: z:/GitHub/rpkgs/spInterp/inst/exec
#> List of 2
#>  $ coord    :Classes 'data.table' and 'data.frame':  2308 obs. of  2 variables:
#>   ..$ x: num [1:2308] 70.5 71.5 72.5 73.5 74.5 75.5 76.5 77.5 78.5 79.5 ...
#>   ..$ y: num [1:2308] 54.5 54.5 54.5 54.5 54.5 54.5 54.5 54.5 54.5 54.5 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ predicted:Classes 'data.table' and 'data.frame':  2308 obs. of  1 variable:
#>   ..$ RH: num [1:2308] 144 139 134 130 126 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "class")= chr "spInterp"
#> 
#> [data] ----------------
#> class       : SpatRaster 
#> dimensions  : 40, 70, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 70, 140, 15, 55  (xmin, xmax, ymin, ymax)
#> coord. ref. :  
#> source(s)   : memory
#> name        :      RH 
#> min value   :  11.127 
#> max value   : 408.459
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
#> 1 Fold1 0.865      0 0.749 0.736 0.862  6.18  4.29  0.54      0.008 0.929   168
#> 2 Fold2 0.91       0 0.828 0.826 0.895  5.60  4.16 -0.492    -0.007 0.952   168
#> 3 Fold3 0.92       0 0.846 0.844 0.909  5.11  3.68  0.08      0.001 0.958   168
#> 4 Fold4 0.925      0 0.855 0.853 0.87   4.71  3.52 -0.359    -0.005 0.958   168
#> 5 Fold5 0.916      0 0.839 0.833 0.903  5.13  3.86  0.848     0.013 0.955   168
#> 6 all   0.907      0 0.823 0.821 0.891  5.37  3.90  0.123     0.002 0.951   840
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
  range = range, res = res, cdd = 450
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==============                                                        |  20%  |                                                                              |============================                                          |  40%  |                                                                              |==========================================                            |  60%  |                                                                              |========================================================              |  80%  |                                                                              |======================================================================| 100%
r
#> # A tibble: 6 × 12
#>   kfold     R pvalue    R2   NSE   KGE  RMSE   MAE   Bias Bias_perc    AI n_sim
#>   <chr> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>     <dbl> <dbl> <int>
#> 1 Fold1 0.869      0 0.755 0.749 0.855  6.03  4.22  0.398     0.006 0.929   168
#> 2 Fold2 0.904      0 0.818 0.816 0.873  5.75  4.21 -0.436    -0.006 0.948   168
#> 3 Fold3 0.924      0 0.854 0.853 0.873  4.85  3.70  0.169     0.003 0.958   166
#> 4 Fold4 0.929      0 0.863 0.854 0.839  4.70  3.56 -0.754    -0.011 0.956   168
#> 5 Fold5 0.914      0 0.836 0.831 0.871  5.16  3.94  0.856     0.013 0.952   168
#> 6 all   0.907      0 0.823 0.823 0.865  5.32  3.92  0.046     0.001 0.949   838
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

``` r
## mask china
# shp <- vect("data-raw/shp/bou1_4p.shp")
# ra = rast(ans) %>% mask(shp)
# plot(ra)
# plot(shp, add = TRUE, col = "transparent")

## 原数据
# p <- ggplot(dat, aes(lon, lat, color = RH)) +
#   geom_point() +
#   scale_color_gradientn(colours = rcolors::get_color("amwg256", 12))
# write_fig(p, "Rplot2.pdf", 10, 5)
```
