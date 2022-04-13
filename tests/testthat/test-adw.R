test_that("spInterp_adw works", {
  library(data.table)
  
  data(TempBrazil) # Temperature for some poins of Brazil
  
  loc <- TempBrazil[, 1:2] %>% set_names(c("lon", "lat"))
  dat <- TempBrazil[, 3] %>% as.matrix()  # Vector with observations in points
  
  range <- c(-78, -34, -36, 5)
  res = 1
  # weight <- weight_adw(loc, range = range, res = 1)
  r = spInterp_adw(loc, dat[, 1], range, res = res, cdd = 450)
  
  expect_true(is.matrix(r$predicted))
  expect_true(dim(r$predicted)[2] == dim(dat)[2])
  expect_equal(r$weight[is.na(w), ] %>% nrow, 0) # has no NA value in weight
  
  # weight <- weight_adw(loc, range = range, res = 1)
  r2 = spInterp_adw(loc, dat, range, res = 5, cdd = 450, fun.weight = "weight_adw_sf")
  names_weight = c("lon", "lat", "I", "dist", "angle", "w")
  expect_equal(r$weight %>% colnames(), names_weight)
})
