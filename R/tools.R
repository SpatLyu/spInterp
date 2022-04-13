#' @export
make_grid <- function(range, res = 0.5) {
  lon <- seq(range[1] + res / 2, range[2], res)
  lat <- seq(range[3] + res / 2, range[4], res)
  expand.grid(lon = lon, lat = lat) %>% cbind(I = 1:nrow(.)) %>% data.table()
}

#' @export
make_dims <- function(range, res = 0.5) {
  lon <- seq(range[1] + res / 2, range[2], res)
  lat <- seq(range[3] + res / 2, range[4], res)
  
  coord = expand.grid(lon = lon, lat = lat) %>% cbind(I = 1:nrow(.)) %>% data.table()
  listk(lon, lat, coord)
}
