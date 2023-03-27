## option1: worked
# 把数据转为matrix
# convert into rast

## option2: not work
# grid = rast(coord)
# rast(grid, nlyrs = ncol(data), vals = data)

# #' @importFrom terra rast
# mat2rast <- function(coord, data) {
#   rast(cbind(as.matrix(coord), data), type = "xyz")
# }

# interp2rast <- function(r, mask = NULL) {
#   mat2rast(r$coord, r$predicted)
# }

# extractByPoint <- function(ra, loc) {
#   points <- vect(loc)
#   terra::extract(ra, points)
# }

make_rast <- function(
    range = c(-180, 180, -90, 90), cellsize = 1, nlyrs = 1, ...) 
{
  if (length(cellsize) == 1) {
    cellsize <- rep(cellsize, 2)
  }
  e <- ext(range[1], range[2], range[3], range[4])
  rast(e, res = cellsize, nlyrs = nlyrs, ...)
}

#' make_grid
#' @keywords internal
#' @export
make_grid <- function(range, res = 0.5) {
  lon <- seq(range[1] + res / 2, range[2], res)
  lat <- seq(range[3] + res / 2, range[4], res)
  expand.grid(lon = lon, lat = lat) %>%
    cbind(I = 1:nrow(.)) %>%
    data.table()
}

#' @rdname make_grid
#' @export
make_dims <- function(range, res = 0.5) {
  lon <- seq(range[1] + res / 2, range[2], res)
  lat <- seq(range[3] + res / 2, range[4], res)
  
  coord = expand.grid(lon = lon, lat = lat) %>% cbind(I = 1:nrow(.)) %>% data.table()
  listk(lon, lat, coord)
}

#' @importFrom terra as.array
rast_matrix <- function(r) {
  as.array(r)[, , 1] %>% fliplr() %>% t()
}

fliplr <- function(x) {
  I <- nrow(x):1
  ndim <- length(dim(x))
  if (ndim == 2) {
    x[I, ]
  } else if (ndim == 3) {
    x[I, , ]
  }
}
