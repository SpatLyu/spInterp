#' Spatial interpolation
#' 
#' @description 
#' The irregularly-spaced data are interpolated onto regular latitude-longitude
#' grids by weighting each station according to its distance or angle from the
#' center of the search radius `cdd`.
#' 
#' @inheritParams cal_weight
#' 
#' @param dat matrix, `[npoint, ntime]`, the observed data used to interpolate grid
#' @param fun.weight function to calculate weight, one of 
#' `c("cal_weight", "cal_weight_sf")`.
#' @param weight predefined weight to speed-up calculation, which is returned 
#' by [spInterp()] itself.
#' @param Z covariates, not used
#' 
#' @author Dongdong Kong and Heyang Song
#' @references 
#' 1. Xavier, A. C., King, C. W., & Scanlon, B. R. (2016). Daily gridded 
#'    meteorological variables in Brazil (1980-2013). International Journal of 
#'    Climatology, 36(6), 2644-2659. <doi:10.1002/joc.4518>
#' 
#' @seealso [wFUN()], [cal_weight()]
#' @example R/examples/ex-adw.R
#' 
#' @importFrom data.table as.data.table merge.data.table setkeyv
#' @export
spInterp <- function(points, dat, range, res = 1, 
  fun.weight = c("cal_weight", "cal_weight_sf"), 
  wFUN = c("wFUN_adw", "wFUN_idw", "wFUN_thiessen", "wFUN_mean"), 
  .parallel = FALSE, 
  ..., 
  Z = NULL, 
  weight = NULL) 
{
  if (!is.matrix(dat)) dat %<>% as.matrix()
  if (nrow(points) != nrow(dat)) stop("Length of points and dat should be equal!")
  fun.weight = match.arg(fun.weight) %>% get()
  wFUN = match.arg(wFUN) #%>% get()
  
  if (is.null(weight)) {
    # pre-define weight will speed up calculation
    l = fun.weight(points, range, res, wFUN = wFUN, ...)
    weight = do.call(rbind, l) %>% as.data.table()  
  }
  grid = weight[, .(lon, lat)] %>% unique() %>% setkeyv(c("lon", "lat"))
  
  ntime = ncol(dat)
  names = colnames(dat) %||% 1:ntime
  
  pred = plyr::llply(set_names(1:ntime, names), function(i) {
    d = cbind(I = 1:nrow(dat), x = dat[, i])
    merge(weight[, .(lon, lat, I, w)], d, by = "I", sort = FALSE) %>% 
      .[, .(value = weighted.mean(x, w, na.rm = TRUE)), .(lon, lat)] %>% 
      { merge(grid, ., all.x = TRUE)$value }
  }, .parallel = .parallel, .progress = "text") %>% do.call(cbind, .)
  
  list(weight = weight, coord = grid, predicted = pred) %>% set_class("spInterp")
}

#' @rdname spInterp
#' @export
spInterp_adw <- function(points, dat, range, res = 1, 
  fun.weight = c("cal_weight", "cal_weight_sf"), ...) {
  spInterp(points, dat, range, res, fun.weight, wFUN = "wFUN_adw", ...)
}
