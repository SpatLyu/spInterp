set.seed(1)
range = c(70, 140, 15, 55)
grid = coords_from_range(range, cellsize = 2)
grid_target = coords_from_range(range, cellsize = 1)

nlat  <- length(grid$lat)
nlon <- length(grid$lon)
ntime <- 10

z <- array(rnorm(nlon*nlat*ntime), c(nlon, nlat, ntime))

z_bl  <- spInterp_bilinear(z, grid, grid_target)
image(z[,,1])
image(z_bl[,,1])

# make_rast(range, cellsize = 2, vals = z)
# rast(z) %>% disagg(fact = 2, method = "bilinear")
