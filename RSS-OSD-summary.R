library(soilDB)
library(aqp)
library(sharpshootR)
library(terra)
library(sf)
library(ragg)


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

agg_png(file = 'RSS-OSDs-ST-dend.png', width = 1800, height = 900, scaling = 1.5)
SoilTaxonomyDendrogram(osds, cex.taxon.labels = 1, KST.order = FALSE, width = 0.35, name.style = 'center-center', y.offset = 0.4, hz.distinctness.offset = 'hzd', shrink = TRUE, depth.axis = list(line = -4))
dev.off()



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

par(mar = c(3.5, 0, 0, 0), lend = 1)

plotSPC(o.sub, width = 0.33, name.style = 'center-center', hz.distinctness.offset = 'hzd', shrink = TRUE, id.style = 'top', cex.names = 0.75, plot.order = idx, cex.id = 0.66, max.depth = 180, depth.axis = list(line = -6, cex = 1, interval = 20))

addBracket(b, offset = -0.45, tick.length = 0, lwd = 6, col = 'royalblue')

axis(side = 1, at = 1:length(o.sub), labels = round(o.sub$proportion[idx], 2), line = 0, cex.axis = 1)

mtext('Approximate Area Proportion within Coweeta', side = 1, line = 2.3, at = 0.5, adj = 0, font = 2)

dev.off()


## RSS horizon data
s <- readRDS('data/combined-tab-data-SPC.rds')

s <- subset(s, source == 'RSS')

s <- unique(s, vars = c('compname', 'hzdept_r', 'hzdepb_r'))

site(s) <- co.area

# re-order according to component area 
s.idx <- order(s$comp.ac, decreasing = TRUE)


ragg::agg_png(filename = 'RSS-components-awc_r.png', width = 2200, height = 900, scaling = 1.75)

par(mar = c(4, 0, 3, 0))

plotSPC(s, label = 'compname', color = 'awc_r', col.label = 'Plant Available Water Holding Capacity (cm/cm)', width = 0.33, name.style = 'center-center', shrink = TRUE, id.style = 'top', cex.names = 0.75, plot.order = s.idx, cex.id = 0.66, max.depth = 180, depth.axis = list(line = -6, cex = 1, interval = 20))

axis(side = 1, at = 1:length(s), labels = round(s$proportion[s.idx], 2), line = 0, cex.axis = 1)

mtext('Approximate Area Proportion within Coweeta', side = 1, line = 2.5, at = 0.5, adj = 0, font = 2)

dev.off()
