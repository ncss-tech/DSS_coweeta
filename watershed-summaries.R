##

## idea: generate excess cLHS points, see which watershed has the most

## TODO: use rassta package




library(raster)
library(dismo)
library(rasterVis)
library(sf)
library(lattice)
library(tactile)
library(viridisLite)
library(hexbin)
library(RColorBrewer)

## raster data


# UTM z17 
e <- raster('grids/elev_pcs.tif')
dah <- raster('grids/DAH.tif')
abr <- raster('grids/beam_rad_sum_mj.tif')
swi <- raster('grids/SWI.tif')
slope_gen <- raster('grids/slope_gen_pct.tif')
mcurv_gen <- raster('grids/mcurv_gen.tif')
asm <- raster('grids/texture15__ASM.tif')
entr <- raster('grids/texture15__Entr.tif')
contr <- raster('grids/texture15__Contr.tif')
forms <- raster('grids/forms.tif')

# RSS
aws050 <- raster('grids/rss_aws050.tif')
taxpartsize <- raster('grids/rss_taxpartsize.tif')


## quick question: ABR vs. DAH
# 35 minutes for ABR
# <1 minute for DAH

z <- sampleRegular(dah, size = 10000, sp = TRUE)
z$abr <- extract(abr, z)
z <- as.data.frame(z)

# 0.9
cor(z$abr, z$DAH, use = 'complete.obs', method = 'spearman')

hexbinplot(
  abr ~ DAH, 
  data = z, 
  xbins = 60,
  colorkey = FALSE, 
  trans = log,
  inv = exp,
  colramp = mako, 
  xlab = 'Diurnal Anisotropic Heat Index', 
  ylab = 'Estimated Annual Beam Radiance (MJ/m^2)', 
  sub = list('Annual Beam Radiance: GRASS GIS r.sun -- Dirunal Aniso. Heat: SAGA GIS', cex = 0.66, font = 1),
  scales = list(alternating = 3, tick.number = 8),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.hexbinplot(...)
  }
  
)


## TODO: missing "flats" (value = 1)
cols.geomorphons <- c(brewer.pal(9, 'Spectral'))
forms <- ratify(forms)
levelplot(forms, att = 'ID', col.regions = cols.geomorphons)



## vector data

# Coweeta watersheds
# UTM z17
b <- read_sf('vect/Coweeta_Hydrologic_Laboratory.shp')
# watershed ID
b$WS <- factor(b$WS)

# sensor locations
# WGS84
p <- read_sf('vect/Soil_Moisture_Sites_CW.shp')
p <- st_transform(p, crs(b))

# make subset of most likely candidates for sensor install
b.sub <- b[b$WS %in% c("7", "32"), ]

# entire watershed as single polygon
b.ws <- st_as_sf(st_union(b))
b.ws$WS <- 'ALL'



## samples for raster extraction

# target watersheds
s <- st_as_sf(st_sample(b.sub, size = 1000, type = 'hexagonal'))

# all of Coweeta
s.all <- st_as_sf(st_sample(b.ws, size = 10000, type = 'hexagonal'))

# spatial intersection for watershed ID
s <- st_intersection(b, s)
s.all <- st_intersection(b.ws, s.all)

# check
par(mar = c(0, 0, 0, 0))
plot(st_geometry(b.ws))
plot(st_geometry(s), add = TRUE, col = 'red', cex = 0.5)

#
plot(taxpartsize)
plot(st_geometry(b), add = TRUE)
plot(st_geometry(s), add = TRUE, cex = 0.4, pch = 16)


## raster list for simpler extraction
# all the same extent / CRS

r.list <- list(
  elev = e, 
  abr = abr,
  slope_gen = slope_gen,
  mcurv_gen = mcurv_gen,
  swi = swi,
  asm = asm,
  contr = contr,
  entr = entr,
  aws050 = aws050,
  taxpartsize = taxpartsize
)

# sample target watersheds + all Coweeta
for(i in names(r.list)) {
  .r <- r.list[[i]]
  s[[i]] <- extract(.r, s)
  s.all[[i]] <- extract(.r, s.all)
}


# column names
vars <- c('WS', names(r.list))

# stack
g <- make.groups(
  'Coweeta' = st_drop_geometry(s.all)[, vars], 
  '7 & 32' = st_drop_geometry(s)[, vars]
)


# fixing factor levels
l <- as.character(levels(taxpartsize)[[1]]$category)
l <- l[-1]

g$taxpartsize <- factor(g$taxpartsize, labels = l)
g$taxpartsize <- factor(g$taxpartsize, levels = c('loamy', 'coarse-loamy', 'fine-loamy'))

# wide -> long format
m <- reshape2::melt(g, id.vars = c('WS'), measure.vars = names(r.list)[1:9])

bwplot(WS ~  value| variable, 
       data = m, 
       xlab='',
       par.settings=tactile.theme(layout.heights=list(strip=1.2)), 
       scales=list(alternating=3, x=list(relation='free')), 
       layout=c(2, 5), 
       as.table=TRUE
)



tab <- table(watershed = g$WS, taxpartsize = g$taxpartsize)
round(prop.table(tab), 2)

# plot(tab, shade = TRUE, color = TRUE)



## bioclim based similarity surface

# continuous raster variables for modeling
rs <- stack(
  list(
    elev = e, 
    abr = abr,
    slope_gen = slope_gen,
    mcurv_gen = mcurv_gen,
    swi = swi,
    asm = asm,
    contr = contr,
    entr = entr
  )
)

# crop to Coweeta BBOX
rs <- crop(rs, b)

# samples from Coweeta
# must be a SPDF
sp.all <- as(s.all[, names(rs)], 'Spatial')
# samples from target watersheds
sp <- as(s[, names(rs)], 'Spatial')


# develop bioclim models
bc.all <- bioclim(rs, sp.all)
bc <- bioclim(rs, sp)

# predictions
p.bc.all <- predict(rs, bc.all, progress = 'text')
p.bc <- predict(rs, bc, progress = 'text')

## TODO: think about this some more
# simple ratio
bc.ratio <- p.bc / p.bc.all

png(filename = 'similarity-surface-7_32.png', width = 800, height = 900, res = 100)

levelplot(
  bc.ratio, 
  scales = list(draw = FALSE), 
  col.regions = viridis,
  margin = FALSE, 
  max.pixels = 1e6,
  main = 'Similarity Surface (7 & 32)',
  panel = function(...) {
    panel.levelplot(...)
    sp.polygons(as(b, 'Spatial'), col = 'white', lwd = 1, lty = 2)
    sp.polygons(as(b.sub, 'Spatial'), col = 'white', lwd = 2)
    sp.points(as(p, 'Spatial'), col = 'white', pch = 16, cex = 0.5)
  }
) 

dev.off()






