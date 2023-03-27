test_that("anusplin works", {  

  dat <- dat_RH[, .(lon, lat, alt, RH)]
  X <- dat[, .(lon, lat, alt)] %>% as.matrix()
  Y <- dat[, .(RH)] %>% as.matrix()

  f_alt <- system.file("anusplin_demo/china_dem_025deg.txt", package = "spInterp")
  range <- c(69.625, 140.375, 14.625, 55.375)
  # range <- c(70, 140, 15, 55) # 高程数据的range有误
  
  outdir = "output"
  # 考虑高程
  expect_no_error({  
    anusplin_make_param(dat, "RH", range, file.alt = f_alt) -> param
    anusplin_write_setting(param, outdir, is.run = FALSE, overwrite = TRUE)
    # skip_if_not(.Platform$OS.type == "windows")
    # anusplin_read_output(param$lapgrd, outdir) -> res
  })
  
  ## 不考虑高程
  # expect_no_error({
  #   anusplin_make_param(dat, "RH", range, file.alt = NULL) -> param
  #   anusplin_write_setting(param, outdir, is.run = TRUE, overwrite = TRUE)
  #   anusplin_read_output(param$lapgrd, outdir) -> res
  # })
})
