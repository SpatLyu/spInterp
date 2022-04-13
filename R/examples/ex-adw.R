library(ggplot2)

data(TempBrazil) # Temperature for some poins of Brazil

loc <- TempBrazil[, 1:2] %>% set_names(c("lon", "lat"))
dat <- TempBrazil[, 3] %>% as.matrix()  # Vector with observations in points

range <- c(-78, -34, -36, 5)
res = 1
# weight <- weight_adw(loc, range = range, res = 1)
r = spInterp_adw(loc, dat, range, res = res, cdd = 450)
print(str(r))

df = r %$% cbind(coord, value = predicted[, 1])
ggplot(df, aes(lon, lat)) +
  geom_raster(aes(fill = value)) +
  geom_point(data = loc, size = 0.5, shape = 3, color = "red") +
  lims(x = range[1:2], y = range[3:4])

r_sf = spInterp_adw(loc, dat, range, res = res, cdd = 450, fun.weight = "weight_adw_sf")
df_sf = r_sf %$% cbind(coord, value = predicted[, 1])
ggplot(df_sf, aes(lon, lat)) +
  geom_raster(aes(fill = value)) +
  geom_point(data = loc, size = 0.5, shape = 3, color = "red") +
  lims(x = range[1:2], y = range[3:4])

# set cdd = 1000
r = spInterp_adw(loc, dat, range, res = res, cdd = 1000)
df = r %$% cbind(coord, value = predicted[, 1])

ggplot(df, aes(lon, lat)) +
  geom_raster(aes(fill = value)) +
  geom_point(data = loc, size = 2.5, shape = 3, color = "red") + 
  lims(x = range[1:2], y = range[3:4])
