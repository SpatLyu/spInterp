\dontrun{
dat <- dat_RH[, .(lon, lat, alt, RH)]
X <- dat[, .(lon, lat, alt)] %>% as.matrix()
Y <- dat[, .(RH)] %>% as.matrix()

res <- 1
range <- c(70, 140, 15, 55)
r_dem <- get_chinadem(res = res) %>% round(1)
f_dem <- "output/dem.asc"
write_dem(r_dem, f_dem, digits = 1)

outdir <- "output"

anusplin_make_param(dat, "RH", range, res, file.alt = f_dem, cvt.coef = 1e3) -> param
anusplin_write_setting(param, outdir, is.run = TRUE, overwrite = TRUE)
anusplin_read_output(param$lapgrd, outdir) -> ans

ans
}
