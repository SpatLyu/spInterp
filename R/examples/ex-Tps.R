## `spInterp_Tps` example -------------------------
dat <- dat_RH[, .(lon, lat, alt, RH)]
X <- dat[, .(lon, lat)] %>% as.matrix()
Y <- dat[, .(RH)] %>% as.matrix()
Z <- dat[, .(alt)] %>% as.matrix()

range <- c(70, 140, 15, 55)
res = 2 # resolution

# prepare dems
r_dem <- get_chinadem(res = res)
ZGrid <- rast2zgrid(r_dem)$ZGrid # a 3d array
image.plot(ZGrid[,,1]) # should looks normal

r = spInterp_Tps(X, Y, range, res = res, Z = Z, ZGrid = ZGrid)
print(r)
plot(r)

# with dem
kfold_ml(X, Y, FUN = spInterp_Tps, 
  range = range, res = res, Z = Z, ZGrid = ZGrid)

# without dem
kfold_ml(X, Y, FUN = spInterp_Tps, 
  range = range, res = res, Z = NULL, ZGrid = ZGrid)
