library(terra)
library(sf2)
library(Ipaper)

r_target = make_rast(range = c(-180, 180, -60, 90), cellsize = 0.5)

f = system.file("inst/extdata/PMLV2_veg-dynamic_GPP_2014.tif", package = "nctools")
r = rast(f)
r2 = resample(r, r_target, "bilinear")

Ipaper::write_fig({
  par(mfrow = c(2, 1))
  plot(r)
  plot(r2)
}, "a.pdf", 10, 8)

## bilinear interpolation
# make sure 3d array
arr = rast_array(r) %>%set_dim(c(dim(.), 1))

d = coords_from_range(cellsize = 1)$loc %>% as.data.table() %>% cbind(z = c(arr))

arr2 = spInterp_bilinear(arr,
                  coords_from_range(cellsize = 1),
                  coords_from_range(cellsize = 0.5), na.rm = TRUE)

Ipaper::write_fig({
  par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
  image(arr[,,1])
  image(arr2[,,1])
}, "bilinear-2.pdf", 10, 8)
