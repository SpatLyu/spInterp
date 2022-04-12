# ' @param z A vector (N) with the values observeds in the points

#' Weights of Angular Distance Weighting interpolation
#'
#' The irregularly-spaced data are interpolated onto regular latitude-longitude
#' grids by weighting each station according to its distance and angle from the
#' center of a search radius.
#' 
#' @param points A matrix (N,2) with longitude and latitude of points of data observed 
#' @param range `[xmin, xmax, ymin, ymax]`
#' @param res the grid resolution
#' @param cdd the correlation decay distance (km), default (450)
#' @param m distance weight (default 4)
#' 
#' @param n.station Number of stations used per point for interpolation. The default is 8.
#' @param ... other parameters to [plyr::ldply]
#' 
#' @return A data.frame with longitude, latitude and interpoled points
#'
#' @author Dongdong Kong and Heyang Song
#' @references 
#' 1. Xavier, A. C., King, C. W., & Scanlon, B. R. (2016). Daily gridded 
#'    meteorological variables in Brazil (1980–2013). International Journal of 
#'    Climatology, 36(6), 2644–2659. <doi:10.1002/joc.4518>
#' 
#' @example R/examples/ex-adw.R
#' @export
weight_adw <- function(points, range, res = 0.5, 
  cdd = 450, m = 4, nstation.max = 8, nstation.min = 3, 
  .progress = "text", ...) 
{
  points %<>% check_matrix()
  sites = points # backup
  
  st_LON = points[, 1] 
  st_LAT = points[, 2]
  .INDEX = INDEX = seq_along(st_LON)

  nsite = nrow(points)
  
  grid = make_grid(range, res)
  ngrid = nrow(grid)
  nstation.max <- pmin(nsite, nstation.max)
  
  iterators = (1:ngrid) %>% set_names(., .)
  plyr::llply(iterators, function(i) {
    lon <- grid$lon[i]
    lat <- grid$lat[i]

    l_1deg = rdist.earth(c(lon, lat), c(lon, lat+1))[1] # 1deg
    delta_deg = cdd / l_1deg * 1.2 # cdd convert to deg, for large data
    range2 <- c(lon, lon, lat, lat) + c(-1, 1)*delta_deg
    
    point <- c(lon, lat)
    # point <- data.frame(x = lon, y = lat) # Data.frame com as coordenadas do ponto a estimar
    # dist <- distance(from = point, to = points) # Dist?ncia euclidianda entre os pontos
    
    if (nsite >= 200) {
      .subset = which(st_LON >= range[1] & st_LON <= range2[2] & 
                      st_LAT >= range[3] & st_LAT <= range2[4])
      if (length(.subset) < nstation.min) return(NULL)
      sites = points[.subset, ]
      .INDEX = INDEX[.subset]
    }
    
    .dist <- rdist.earth(point, sites)
    .ind <- findn_small(.dist, nstation.max)

    valid = which(.dist[.ind] <= cdd)
    if (length(valid) < nstation.min) return(NULL)
    
    ind = .ind[valid]
    dist = .dist[ind]
    
    coef <- (exp((-dist) / cdd))**m # Xavier 2016, Eq. 7
    theta <- .bearing(c(lon, lat), sites[ind, ]) %>% deg2rad()
  
    alpha <- sapply(seq_along(dist), function(k) {
      diffTheta <- theta[-k] - theta[k]
      sum(coef[-k] * (1 - cos(diffTheta))) / sum(coef[-k]) # Xavier 2016, Eq 8
    })
    w = coef * (1 + alpha) 
    # W = W/sum(W)
    # pred <- sum(W * z[ind]) / sum(W)
    # data.frame(lon, lat, value = pred)
    data.table(lon, lat, I = .INDEX[ind], dist = dist, angle = rad2deg(theta), w)
  }, .progress = .progress, ...) %>% set_names(iterators)
}


#' @examples
#' set.seed(2)
#' dd <- data.frame(
#'   lon = runif(100, min = 110, max = 117),
#'   lat = runif(100, min = 31, max = 37),
#'   value = runif(100, min = -10, max = 10)
#' )
#' head(dd)
#' dg <- adw(dd, gridSize = 1, cdd = 1e5)
#' # dg is the dataframe of grid (mesh)
#' head(dg)
#' 
#' @importFrom sf st_as_sf st_buffer st_coordinates st_distance st_geometry
#' @importFrom stats na.omit
#' @importFrom data.table data.table
#' 
#' @rdname weight_adw
#' @export
weight_adw_sf <- function(points, range = NULL, res = 0.25, 
  cdd = 450, m = 4, nstation.max = 8, nstation.min = 3, 
  .progress = "text", ...) 
{
  if (is.null(range)) range = c(min(points$lon), max(points$lon), min(points$lat), max(points$lat))
  
  coord <- make_grid(range, res)
  grid <- cbind(st_as_sf(coord, coords = c("lon", "lat"), crs = 4326), coord)
  ngrid = nrow(grid)

  sf_points <- points %>% cbind(I = 1:nrow(.), .) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) # add a Id column

  iterators = (1:ngrid) %>% set_names(., .)
  plyr::llply(iterators, function(i) {
    loc_i <- grid[i, ]
    lon <- loc_i$lon
    lat <- loc_i$lat
    
    circle <- st_buffer(loc_i, dist = units::set_units(cdd, "km")) %>% st_geometry()

    dx <- sf_points[circle, ]
    dx$dist <- as.numeric(st_distance(loc_i, dx) / 1e3) # to km
    dx %<>% subset(dist <= cdd)

    npts <- nrow(dx)
    
    if(npts >= 3) {
      n <- min(nstation.max, npts) # selected
      ind = findn_small(dx$dist, n)
      dx = dx[ind, ]
      # if (npts > nstation.max) dx <- dx[order(dx$dist), ] %>% head(n)
      coef  <- exp(-dx$dist / cdd)^m # Xavier 2016, Eq 7
      theta <- .bearing(c(lon, lat), st_coordinates(dx)) %>% deg2rad()
      
      alpha <- sapply(seq_len(n), function(k) {
        diffTheta <- theta[-k] - theta[k]
        sum(coef[-k] * (1 - cos(diffTheta))) / sum(coef[-k]) # Xavier 2016, Eq 8
      })
      w <- coef * (1 + alpha)
      # w = w / sum(w)
      data.table(lon = lon, lat = lat, I = dx$I, dist = dx$dist, angle = rad2deg(theta), w) 
      # data.frame(lon = lon, lat = lat, site = dx$site, wt = w)
    }
  }, .progress = .progress, ...) %>% set_names(iterators)
}


#' spInterp_adw
#' 
#' @param z matrix `[npoint, ntime]`
#' @importFrom data.table merge.data.table
#' @export
spInterp_adw <- function(points, dat, range, res = 1, ...) {
  l = weight_adw(points, range, res, ...)
  weight = do.call(rbind, l) %>% .[, .(lon, lat, I, w)]
  grid = weight[, .(lon, lat)] %>% unique() %>% setkeyv(c("lon", "lat"))
  
  ntime = ncol(dat)
  names = colnames(dat) %||% 1:ntime
  
  pred = lapply(set_names(1:ntime, names), function(i) {
    d = cbind(I = 1:nrow(dat), x = dat[, i])
    merge(weight, d, by = "I", sort = FALSE) %>% 
      .[, .(value = sum(x * w, na.rm = TRUE)), .(lon, lat)] %>% 
      { merge(grid, ., all.x = TRUE)$value }
      # .[, .(lon, lat, value)]
  }) %>% do.call(cbind, .)
  list(weight = weight, coord = grid, predicted = pred)
}
