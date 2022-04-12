\dontrun{
library(CMIP6tools)
# library(rgdal)
# library(sp2)
library(maptools)

# polyline
read_polyline <- function(file, lwd = 0.5, ...){
    suppressWarnings(shp <- readShapeLines(file))
    proj4string(shp) <- prj84
    list("sp.lines", shp, first = FALSE, lwd = lwd, ...) # china poly    
}
# sp_arc_world <- read_polyline("G:/ArcGIS/continent.shp") # sp_arc_world
# sp_arc_CH <- read_polyline(system.file("extdata/shp", "bou1_4l_sml.shp", package = "CMIP5tools"))

range <- c(70, 140, 15, 55)
# MAIN SCRIPTS ------------------------------------------------------------
x <- readRDS(system.file("extdata", "tasmax_Threshold_ACCESS1-0.RDS", package = "CMIP5tools"))

class(x) <- "raster2"
plot(x)

x_interp <- spInterp_bilinear(x$grid, x$TRS, range, cellsize_x = 1.5) 
x_clip <- clip_raster2(x, range)

plot(x_interp)
plot(x_clip) # , sp.layout = sp_arc_CH
}
