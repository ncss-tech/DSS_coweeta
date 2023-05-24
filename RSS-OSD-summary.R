library(soilDB)
library(aqp)
library(sharpshootR)
library(terra)
library(sf)


# RSS: cell values are map unit keys
# has a simple raster attribute table
r <- rast('grids/rss_utm.tif')

# RSS tabular data
load('data/rss-tab-data-raw.rda')



## approximate acreage
## pixel-based estimates of area (largest, non-misc. area)
mu.area <- na.omit(as.data.frame(freq(r)))
mu.area$ac <- mu.area$count * (mean(res(r))^2) * 0.000247105

a <- merge(mu.area, rss.co[rss.co$compkind == 'Series', ], by.x = 'value', by.y = 'mukey', sort = FALSE, all.x = TRUE)
a$comp.ac <- a$ac * (a$comppct_r / 100)

co.area <- aggregate(comp.ac ~ compname, data = a, FUN = sum)
co.area <- co.area[order(co.area$comp.ac, decreasing = TRUE), ]

# proportion
co.area$proportion <- co.area$comp.ac / sum(co.area$comp.ac)

# investigate named components
osds <- fetchOSD(co.area$compname)

# join to SPC
co.area$id <- toupper(co.area$compname)
site(osds) <- co.area[, c('id', 'comp.ac', 'proportion')]

s <- site(osds)

SoilTaxonomyDendrogram(osds, width = 0.35, name.style = 'center-center', y.offset = 0.4, hz.distinctness.offset = 'hzd', shrink = TRUE)


o.sub <- osds

# re-order according to component area 
idx <- order(o.sub$comp.ac, decreasing = TRUE)

## plant-available water storage interval
## top of mineral soil to 100cm or contact

# get depth to contact
sdc <- getSoilDepthClass(osds)
site(o.sub) <- sdc

# get top depth of mineral soil
o.sub$mineral.top <- profileApply(o.sub, function(i) {
  # remove any organic horizons
  .h <- horizons(i)
  .h <- .h[grep('O', x = .h$hzname, invert = TRUE), ]
  
  # select top depth of first mineral horizon
  .r <- min(.h$top)
  return(.r)
})

# get shallower: contact or mineral top + 100cm
o.sub$mineral.bottom <- pmin(o.sub$depth, o.sub$mineral.top + 100)


# re-package for depth brackets
b <- data.frame(id = profile_id(o.sub), top = o.sub$mineral.top, bottom = o.sub$mineral.bottom)


ragg::agg_png(filename = 'RSS-OSDs-no-lines.png', width = 2200, height = 900, scaling = 1.75)

par(mar = c(1.5, 0, 0, 0), lend = 1)

plotSPC(o.sub, width = 0.33, name.style = 'center-center', hz.distinctness.offset = 'hzd', shrink = TRUE, id.style = 'top', cex.names = 0.75, plot.order = idx, cex.id = 0.66, max.depth = 170, axis.line.offset = -6, cex.depth.axis = 1, n.depth.ticks = 10, plot.depth.axis = TRUE)

addBracket(b, offset = -0.45, tick.length = 0, lwd = 6, col = 'royalblue')

axis(side = 1, at = 1:length(o.sub), labels = round(o.sub$proportion[idx], 2), line = -2, cex.axis = 1)

mtext('Approximate Area Proportion within Coweeta', side = 1, line = 0, at = 0.5, adj = 0, font = 2)

dev.off()



