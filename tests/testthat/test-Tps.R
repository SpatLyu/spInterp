test_that("multiplication works", {
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
  image.plot(ZGrid[, , 1]) # should looks normal

  r = spInterp_Tps(X, Y, range, res = res, Z = Z, ZGrid = ZGrid)
  expect_true(all(as.vector(ext(as_rast(r))) == range))
  expect_no_error({
    print(r)
    plot(r)
  })
  
  # kfold
  r2 = kfold_ml(X, Y,
    FUN = spInterp_Tps,
    range = range, res = res, Z = Z, ZGrid = ZGrid
  )
  expect_true(r2$gof$R2[6] >= 0.78)
})
