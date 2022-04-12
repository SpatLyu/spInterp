#' @export
findn_small <- function(x, n = 10, decreasing = FALSE, fill.na = TRUE) {
  N = length(x)
  n2 = pmin(N, n)
  
  o <- order(x, decreasing = decreasing)
  if (!fill.na) {
    o[1:n2]
  } else {
    c(o[1:n2], rep(NA, n - n2))
  }
}

#' @export
make_grid <- function(range, res = 0.5) {
  lon <- seq(range[1] + res / 2, range[2], res)
  lat <- seq(range[3] + res / 2, range[4], res)
  expand.grid(lon = lon, lat = lat) %>% cbind(I = 1:nrow(.)) %>% data.table()
}

#' @export
rowvec <- function(x, ...) {
  matrix(x, nrow = 1, ...)
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
