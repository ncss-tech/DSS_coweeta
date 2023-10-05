## Prepare various thematic maps from SSURGO/RSS data.
## 2023-10-05
## D.E. Beaudette

library(aqp)
library(terra)
library(ragg)

## dominant component + map unit data
dominant.cokey.lut <- readRDS('data/dominant-cokey-LUT.rds')

## wt.mean depth to restriction by map unit
rest.depth <- readRDS('data/rest-depth-wtmean-LUT.rds')
names(rest.depth)[2] <- 'wtmean.depth.to.restriction'

# combine LUTs
lut <- merge(dominant.cokey.lut, rest.depth, by = 'mukey', sort = FALSE)


## mukey grids
s <- rast('grids/ssurgo_utm.tif')
r <- rast('grids/rss_utm.tif')

## watersheds for context
w <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

## extract RATs
s.rat <- cats(s)[[1]]
r.rat <- cats(r)[[1]]

## merge with select map unit level properties
.vars <- c('mukey', 'mukind', 'invesintens', 'compkind', 'comppct_r', 'compname', 'depth.class', 'depth', 'resdepth', 'depth.to.restriction', 'wtmean.depth.to.restriction')
s.rat <- merge(s.rat, lut[, .vars], by = 'mukey', all.x = TRUE, sort = FALSE)
r.rat <- merge(r.rat, lut[, .vars], by = 'mukey', all.x = TRUE, sort = FALSE)

## re-order columns, terra requirement
s.rat <- s.rat[, c('ID', .vars)]
r.rat <- r.rat[, c('ID', .vars)]

## re-pack updated RATs
levels(s) <- s.rat
levels(r) <- r.rat

## quick graphical check, categories may not be in order
activeCat(s) <- 'depth.to.restriction'
plot(s)
lines(w)

activeCat(r) <- 'depth.to.restriction'
plot(r)
lines(w)

## save grids + RATs for later
# these are re-loaded properly by terra::rast()
# NA are encoded as "-2147483648" ... is this a problem?
# cannot be directly used by QGIS 
writeRaster(s, filename = 'grids/ssurgo-mu-properties.tif', overwrite = TRUE)
writeRaster(r, filename = 'grids/rss-mu-properties.tif', overwrite = TRUE)


## develop numeric grids for simpler interpretation / encoding of NA
s.depth.to.restriction <- as.numeric(s, index = 'depth.to.restriction')
r.depth.to.restriction <- as.numeric(r, index = 'depth.to.restriction')

# save
writeRaster(s.depth.to.restriction, filename = 'grids/ssurgo-dominant-depth-to-restriction.tif', overwrite = TRUE)
writeRaster(r.depth.to.restriction, filename = 'grids/rss-dominant-depth-to-restriction.tif', overwrite = TRUE)


# combine for simpler plotting
.combined <- c(s.depth.to.restriction, r.depth.to.restriction)

agg_png(filename = 'figures/restrictive-feat-depth-dominant-component.png', width = 1600, height = 950, scaling = 1.75)

par(mfcol = c(1,2))

plot(
  .combined, 
  col = hcl.colors(10, palette = 'zissou1', rev = TRUE),
  type = 'continuous', 
  axes = FALSE, 
  main = c('SSURGO\nDepth to Restrictive Features (cm)', 'RSS\nDepth to Restrictive Features (cm)'), 
  range = c(35, 205), 
  mar = c(1, 2, 1, 4), 
  fun = function() {lines(w); mtext(side = 1, text = 'Dominant Component\nMisc. Areas Removed', line = -2)}
)

dev.off()

## develop numeric grids for simpler interpretation / encoding of NA
s.wtmean.depth.to.restriction <- as.numeric(s, index = 'wtmean.depth.to.restriction')
r.wtmean.depth.to.restriction <- as.numeric(r, index = 'wtmean.depth.to.restriction')

# save
writeRaster(s.wtmean.depth.to.restriction, filename = 'grids/ssurgo-wtmean-depth-to-restriction.tif', overwrite = TRUE)
writeRaster(r.wtmean.depth.to.restriction, filename = 'grids/rss-wtmean-depth-to-restriction.tif', overwrite = TRUE)

# combine for simpler plotting
.combined <- c(s.wtmean.depth.to.restriction, r.wtmean.depth.to.restriction)

agg_png(filename = 'figures/restrictive-feat-depth-wtmean.png', width = 1600, height = 950, scaling = 1.75)

par(mfcol = c(1,2))

plot(
  .combined, 
  col = hcl.colors(10, palette = 'zissou1', rev = TRUE),
  type = 'continuous', 
  axes = FALSE, 
  main = c('SSURGO\nDepth to Restrictive Features (cm)', 'RSS\nDepth to Restrictive Features (cm)'), 
  range = c(35, 205), 
  mar = c(1, 2, 1, 4), 
  fun = function() {lines(w); mtext(side = 1, text = 'Wt. Mean over Components\nMisc. Areas Removed', line = -2)}
)

dev.off()







