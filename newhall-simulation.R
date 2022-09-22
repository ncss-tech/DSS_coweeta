## ideas here:
# https://ncss-tech.github.io/jNSMR/reference/newhall_batch.html
# https://ncss-tech.github.io/jNSMR/articles/newhall-prism.html


library(terra)
library(jNSMR)
library(viridisLite)

# GCS
ppt <- rast('e:/gis_data/prism/final_monthly_ppt_800m.tif')
tavg <- rast('e:/gis_data/prism/final_monthly_tavg_800m.tif')

# UTM
elev <- rast('grids/elev_pcs.tif')
aws <- rast('grids/rss_aws050.tif')

# watershed
w <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

# convert AWS from cm -> mm
aws <- aws * 10

# warp / resample / crop to elevation
ppt <- project(ppt, elev, method = 'bilinear')
tavg <- project(tavg, elev, method = 'bilinear')

# fix raster layer names
.months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
names(tavg) <- sprintf("t%s", .months)
names(ppt) <- sprintf("p%s", .months)
names(elev) <- 'elev'
names(aws) <- 'awc'

# combine all data into single collection
x <- c(tavg, ppt, elev, aws)

# extract cell coordinates
# UTM
xy <- terra::xyFromCell(x[[1]], 1:ncell(x[[1]]))

# convert to WGS84 GCS
xy <- project(xy, from = crs(elev), to = 'EPSG:4326')

# add WGS84 coordinates as new bands
x$lonDD <- xy[, 1]
x$latDD <- xy[, 2]


# check: OK
names(x)



# try at coarser resolution
a <- aggregate(x, fact = 10)

# ~ 25 seconds are 10x reduced res
# ~ 2 minutes full res
system.time(
  sim <- newhall_batch(
    x,
    unitSystem = "metric",
    soilAirOffset = 2,
    amplitude = 0.66,
    verbose = TRUE,
    toString = FALSE,
    checkargs = TRUE,
    cores = 8,
    file = paste0(tempfile(), ".tif"),
    overwrite = TRUE
  )
)

# check
sim

plot(sim[[1:4]], col = mako(25))
plot(sim[[5:8]], col = mako(25))
plot(sim[[9:12]], col = mako(25))
plot(sim[[13:16]], col = mako(25))

plot(sim[[3:4]], col = mako(25), axes = FALSE, legend = 'topright')

plot(sim[[c(8,10)]], col = mako(25), axes = FALSE, legend = 'topright')

# plot(sim[[18]], col = RColorBrewer::brewer.pal(6, 'Spectral'), axes = FALSE, legend = 'topright')
# lines(w)

plot(sim[[19]], col = RColorBrewer::brewer.pal(5, 'Spectral'))
lines(w)

# plotting using named colors is super slow
# rgb(t(col2rgb(c('royalblue', 'orange', 'firebrick'))), maxColorValue = 255)

par(mfcol = c(1, 2))
plot(sim[[16]], col = c("#4169E1", "#FFA500", "#B22222"), axes = FALSE, legend = 'topleft', mar = c(1, 0, 1, 0))
lines(w)

plot(sim[[17]], col = c("#B22222", "#FFA500", "#4169E1"), axes = FALSE, legend = 'topleft', mar = c(1, 0, 1, 0))
lines(w)



