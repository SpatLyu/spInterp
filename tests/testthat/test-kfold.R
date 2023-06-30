test_that("kfold works", {

  data(TempBrazil) # Temperature for some poins of Brazil

  X <- TempBrazil[, 1:2] %>% set_names(c("lon", "lat"))
  Y <- TempBrazil[, 3] %>% as.matrix() # Vector with observations in points
  range <- c(-78, -34, -36, 5)
  res = 1

  r = kfold_ml(X, Y,
    FUN = spInterp_adw,
    range = range, res = res, cdd = 450
  )
  
  expect_equal(nrow(r$gof), 6)
  expect_true(r$gof$R2[6] > 0.50)
})
