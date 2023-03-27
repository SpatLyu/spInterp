#' @export
print.spInterp <- function(x, ...) {
  str(x)
  cat("\n[data] ----------------\n")
  print(as_rast(x))
  invisible()
}

#' @export
as_rast <- function(x, ...) UseMethod("as_rast")

#' @export
as_rast.spInterp <- function(x, ...) {
  d <- cbind(x$coord, x$predicted)
  rast(d)
}

#' @importFrom terra plot
#' @exportS3Method plot spInterp
plot.spInterp <- function(x, ...) {
  r <- as_rast(x)
  plot(r)
}

#' @importFrom stats predict
#' @importFrom terra vect plot
#' @exportS3Method predict spInterp
predict.spInterp <- function(object, data = NULL, ...) {
  # ra <- interp2rast(object, mask = NULL)
  ra <- as_rast(object)
  points <- vect(data[, c("lon", "lat")])
  # rm ID, only one variable left, hence returns vector
  terra::extract(ra, points)[, -1]
}

#' Get China dem rast
#' 
#' @export 
get_chinadem <- function(range = c(70, 140, 15, 55), res = 0.5) {
  f_dem <- system.file("extdata/merit_China_G010_dem.tif", package = "spInterp")
  r_target <- make_rast(range, cellsize = res)
  r_dem <- terra::resample(rast(f_dem), r_target, "average")
  r_dem
}

#' @importFrom terra res ext
#' @export 
rast2zgrid <- function(r) {
  ZGrid <- rast_matrix(r) 
  if (is.matrix(ZGrid)) ZGrid %<>% set_dim(c(dim(.), 1))

  range = as.vector(ext(r))
  res = res(r)
  lon = seq(range[1] + res[1]/2, range[2], res[1])
  lat = seq(range[3] + res[2]/2, range[4], res[2])
  listk(lon, lat, ZGrid)
}
