#' Thin plate spline regression
#' 
#' @inheritParams fields::predictSurface
#' @param ... others to [fields::Tps()]
#' 
#' @example R/examples/ex-Tps.R
#' 
#' @importFrom data.table as.data.table merge.data.table setkeyv
#' @importFrom fields predictSurface Tps
#' 
#' @export
spInterp_Tps <- function(points, dat, range, res = 1, 
  Z = NULL, ZGrid = NULL, 
  # fun.weight = c("cal_weight", "cal_weight_sf"), 
  # wFUN = c("wFUN_adw", "wFUN_idw", "wFUN_thiessen", "wFUN_mean"), 
  .parallel = FALSE, 
  ..., 
  weight = NULL) 
{
  if (!is.matrix(dat)) dat %<>% as.matrix()
  if (nrow(points) != nrow(dat)) stop("Length of points and dat should be equal!")

  if (is.null(Z)) ZGrid = NULL
  
  grid <- query_gridInfo(range, res)
  m = Tps(points, dat, Z = Z, ...)  
  pred = predictSurface(m, grid, extrap = TRUE, ZGrid = ZGrid)

  coord <- expand.grid(lon = grid$lon, lat = grid$lat) %>% data.table()
  vals <- pred$z %>% c()
  
  listk(weight, coord, predicted = vals) %>% set_class("spInterp")
  # cbind(coord, vals)
}

query_gridInfo <- function(range, res) {
  delta = res / 2
  lon = seq(range[1] + delta, range[2], res)
  lat = seq(range[3] + delta, range[4], res)
  listk(lon, lat)
}
