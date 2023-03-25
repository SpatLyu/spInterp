## option1: worked
# 把数据转为matrix
# convert into rast

## option2: not work
# grid = rast(coord)
# rast(grid, nlyrs = ncol(data), vals = data)

#' @importFrom terra rast
mat2rast <- function(coord, data) {
  rast(cbind(as.matrix(coord), data), type = "xyz")
}

interp2rast <- function(r, mask = NULL) {
  mat2rast(r$coord, r$predicted) 
}

extractByPoint <- function(ra, loc) {
  points = vect(loc)
  extract(ra, points)
}
