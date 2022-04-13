
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spInterp: Spatial Interpolation

<!-- badges: start -->

[![R-CMD-check](https://github.com/rpkgs/spInterp/workflows/R-CMD-check/badge.svg)](https://github.com/rpkgs/spInterp/actions)
[![codecov](https://codecov.io/gh/rpkgs/spInterp/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rpkgs/spInterp)
[![CRAN](http://www.r-pkg.org/badges/version/spInterp)](https://cran.r-project.org/package=spInterp)
<!-- [![total](http://cranlogs.r-pkg.org/badges/grand-total/spInterp)](https://www.rpackages.io/package/spInterp)
[![monthly](http://cranlogs.r-pkg.org/badges/spInterp)](https://www.rpackages.io/package/spInterp) -->
<!-- badges: end -->

## Installation

You can install the development version of spInterp from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rpkgs/spInterp")
```

## On top of

1.  <https://github.com/rrodrigojrr/ADW>
2.  <https://github.com/PanfengZhang/adw>

## Performance

``` r
library(spInterp)
library(adw)

data(TempBrazil) # Temperature for some poins of Brazil

loc <- TempBrazil[, 1:2] %>% set_names(c("lon", "lat"))
dat <- TempBrazil[, 3] %>% as.matrix()  # Vector with observations in points

range <- c(-78, -34, -36, 5)

# Compare with the R package adw
dd = cbind(loc, value = dat[,1])
system.time({
  r_adw <- adw::adw(dd, range[1], range[2], range[3], range[4], cdd = 450*1e3, gridSize = 1)  
})
#>  用户  系统  流逝 
#> 60.53  0.28 60.81

system.time({
  r_spInterp <- spInterp_adw(loc, dat, range, res = 1, cdd = 450)
})
#> 用户 系统 流逝 
#> 0.60 0.00 0.59
```

``` r
df = rbind(
  r_spInterp %$% cbind(coord, value = predicted[, 1], method = "spInterp"), 
  cbind(r_adw, method = "adw"))

library(ggplot2)
ggplot(df, aes(lon, lat)) + 
  geom_raster(aes(fill = value)) +
  # geom_point(data = loc, size = 2.5, shape = 3, color = "red") +
  facet_wrap(~method) + 
  lims(x = range[1:2], y = range[3:4])
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be
#> shifted. Consider using geom_tile() instead.
#> Warning: Raster pixels are placed at uneven vertical intervals and will be
#> shifted. Consider using geom_tile() instead.
#> Warning: Removed 170 rows containing missing values (geom_raster).
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

`spInterp` used the exactly same algrithm as that of `adw` package. But
unlike `adw`, `spInterp` avoids using `sf` for spatial data processing
such as buffer, intersect. Surprisingly, `spInterp` is dozens of times
faster.

There is a slight difference from `adw` in the calculation of the
distance between two points on the sphere, because `spInterp` uses self
defined function, while `adw` uses `sf::st_distance`. But the difference
is tiny (might about 1%), can be ignored at most situation.

## References

1.  Xavier, A. C., King, C. W., & Scanlon, B. R. (2016). Daily gridded
    meteorological variables in Brazil (1980–2013). International
    Journal of Climatology, 36(6), 2644–2659. <doi:10.1002/joc.4518>
