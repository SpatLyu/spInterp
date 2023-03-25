library(tidymet)

f = "inst/anusplin_demo/RH_01-01.dat"
f = "data-raw/RH_01-01.dat"
data <- fread(f) %>%
  `colnames<-`(c("site", "lon", "lat", "alt", "RH"))
inds <- match2(st840$site, data$site)$I_y
dat_RH <- data[inds, ]
dat_RH

use_data(dat_RH, overwrite = TRUE)
