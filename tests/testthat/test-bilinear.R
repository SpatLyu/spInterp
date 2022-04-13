test_that("spInterp_bilinear works", {
  arr = PMLV2_2014_gpp
  interp_PML <- function(arr, res = 2, plot = FALSE) {
    range = c(-180, 180, -60, 90)
    dims = make_dims(range, res = 1)
    arr_bl  <- spInterp_bilinear(arr, dims, range, res = res)
    
    if (plot) {
      par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
      image(arr[,,1])
      image(arr_bl[,,1])
    }
    arr_bl
  }
  
  arr_deg2_v0 = interp_PML(arr, res = 2)
  arr_deg2_v1 = interp_PML(arr[,,1], res = 2)
  
  expect_equal(arr_deg2_v0, arr_deg2_v1)
  expect_equal(dim(arr_deg2_v1), c(180, 75, 1))
  
  arr_deg05 = interp_PML(arr, res = 0.5)
  expect_equal(dim(arr_deg05), c(720, 300, 1))
})
