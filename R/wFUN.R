#' @title function to calculate sites weights 
#' @name wFUN
#' 
#' @description 
#' - `wFUN_adw`: angular distance weighting
#' - `wFUN_idw`: inverse distance weighting
#' - `wFUN_mean`: arithmetic average weighting
#' - `wFUN_thiessen`,`wFUN_natural`: natural neighbor interpolation (also known
#'   as thiessen interpolation)
NULL

#' @param dist numeric vector, the distance between point and each sites, with the
#' same length as `sites`.
#' @param point numeric vector `c(lon, lat)`, the point needed to be interpolated
#' @param sites matrix, `cbind(lon, lat)`, coordinates of sites used to interpolate `point`
#' @param ... ignored
#' 
#' @inheritParams cal_weight
#' @rdname wFUN
#' @export
wFUN_adw <- function(dist, m = 4, cdd = 450, point, sites, ...) {
  coef <- (exp((-dist) / cdd))**m # Xavier 2016, Eq. 7
  theta <- .bearing(point, sites) %>% deg2rad()
  theta[dist <= 1e-2] = 0 # site just on the grid

  alpha <- sapply(seq_along(dist), function(k) {
    diffTheta <- theta[-k] - theta[k]
    sum(coef[-k] * (1 - cos(diffTheta))) / sum(coef[-k]) # Xavier 2016, Eq 8
  })
  w = coef * (1 + alpha)
  data.table(angle = rad2deg(theta), w = w)
}

#' @rdname wFUN
#' @export
wFUN_idw <- function(dist, m = 2, ...) {
  w = 1/dist^m
  w / sum(w)
}

#' @rdname wFUN
#' @export
wFUN_mean <- function(dist, ...) {
  rep(1, length(dist), ...)
}

#' @rdname wFUN
#' @export
wFUN_thiessen <- function(dist, ...) {
  w = rep(0, length(dist))
  w[which.min(dist)] = 1
  w
}

#' @rdname wFUN
#' @export
wFUN_natural <- wFUN_thiessen
