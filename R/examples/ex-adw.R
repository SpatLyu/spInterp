library(ggplot2)

data(TempBrazil) # Temperature for some poins of Brazil

loc <- TempBrazil[, 1:2] %>% set_names(c("lon", "lat"))
Temp <- TempBrazil[, 3] # Vector with observations in points

range <- c(-78, -34, -36, 5)

# 
ggplot(r, aes(lon, lat)) +
  geom_raster(aes(fill = value)) +
  geom_point(data = loc, size = 2.5, shape = 3, color = "red")

profvis::profvis({
# proffer::pprof({
# system.time({
  # r <- ADW(xy = loc, z = Temp, xrange = range[1:2], yrange = range[3:4])
  # l <- weight_adw(loc, range = range, res = 2, .progress = "text")
  r <- weight_adw(loc, range = range, res = 1)
})
x = rnorm(100)

system.time({
  r1 = weight_adw(loc, range, res = 0.25)
  # r2 = weight_adw_sf(loc, range, res = 2)
})


compare(
  sapply(r1, nrow) ,
  sapply(r2, nrow)  
)

system.time(
  foreach(i = 1:1e3) %do% {
    cal_hw(c(100, 20))
  }
)

dat = matrix(rnorm(265*10), 265)

# 112
res = spInterp_adw(loc, dat)


