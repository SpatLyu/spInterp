#' Weights for spatial interpolation
#' 
#' @param points A matrix (N,2) with longitude and latitude of points of data observed
#' @param range `[xmin, xmax, ymin, ymax]`
#' @param res the grid resolution (degree)
#' @param cdd the correlation decay distance (km) for `adw` and searching radius 
#' for others, default (450). 
#' @param m distance weight (default 4)
#' 
#' @param nstation.max Number of maximum stations used per point for
#' interpolation, (default 8).
#' @param nstation.min Number of minimum stations used per point for
#' interpolation, (default 3).
#' @param wFUN `wFUN_*` functions, see [wFUN()] for details
#' 
#' @inheritParams plyr::ldply
#' @param ... other parameters to [plyr::ldply]
#' 
#' @return A data.frame with longitude, latitude and interpoled points
#' 
#' @export
cal_weight <- function(points, range, res = 0.5, 
  cdd = 450, m = 4, 
  nstation.max = 8, nstation.min = 3, 
  wFUN = c("wFUN_adw", "wFUN_idw", "wFUN_thiessen", "wFUN_mean"),
  .progress = "none", ...) 
{
  wFUN = match.arg(wFUN) %>% get()
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
    point <- c(lon, lat)

    l_1deg = rdist.earth(c(lon, lat-1), c(lon, lat+1))[1]/2 # 1deg
    delta_deg = cdd / l_1deg * 1.2 # cdd convert to deg, for large data
    range2 <- c(lon, lon, lat, lat) + c(-1, 1, -1, 1)*delta_deg
    
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
    dist[dist <= 1e-2] = 1e-2 # about 10m

    sites_interp = sites[ind, ]
    w = wFUN(dist, m, cdd, point, sites_interp)
    # W = W/sum(W)
    # pred <- sum(W * z[ind]) / sum(W)
    # data.frame(lon, lat, value = pred)
    data.table(lon, lat, I = .INDEX[ind], dist = dist, w)
  }, .progress = .progress, ...) %>% set_names(iterators)
}

#' @importFrom sf st_as_sf st_buffer st_coordinates st_distance st_geometry
#' @importFrom stats na.omit
#' @importFrom data.table data.table
#' 
#' @rdname cal_weight
#' @export
cal_weight_sf <- function(points, range = NULL, res = 0.25, 
  cdd = 450, m = 4, nstation.max = 8, nstation.min = 3, 
  wFUN = c("wFUN_adw", "wFUN_idw", "wFUN_thiessen", "wFUN_mean"),
  .progress = "none", ...) 
{
  wFUN = match.arg(wFUN) %>% get()
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
    point <- c(lon, lat)
    
    circle <- st_buffer(loc_i, dist = units::set_units(cdd, "km")) %>% st_geometry()

    dx <- sf_points[circle, ]
    dx$dist <- as.numeric(st_distance(loc_i, dx) / 1e3) # to km
    dx %<>% subset(dist <= cdd)

    npts <- nrow(dx)
    if(npts >= nstation.min) {
      n <- min(nstation.max, npts) # selected
      ind = findn_small(dx$dist, n)
      dx = dx[ind, ]
      
      dist = dx$dist
      sites_interp = st_coordinates(dx)

      w = wFUN(dist, m, cdd, point, sites_interp)
      data.table(lon = lon, lat = lat, I = dx$I, dist, w) 
    }
  }, .progress = .progress, ...) %>% set_names(iterators)
}
