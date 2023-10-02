## TODO:
## * soil parameter files for each map unit key (dominant component)
## * weighted avg. soil properties to contact / rest. feature
## * use ROSETTA for Ksat and water retention curve points

library(aqp)
library(lattice)
library(tactile)
library(reshape2)

source('local-functions.R')

## combined mu/component data, as SPC
x <- readRDS('data/combined-tab-data-SPC.rds')

## investigate mukind by source
xtabs(~ source + mukind, data = unique(site(x)[, c('mukey', 'source', 'mukind')]))

## restrictive features
r <- diagnostic_hz(x)

# kind
(.tab <- xtabs( ~ reskind + which, data = r))
round(prop.table(.tab, margin = 2), 2)

## TODO: wt. mean soil properties to contact



## original proportions of depth class / by source
xtabs(~ source + depth.class, data = site(x))

histogram(~ depth | source, data = site(x), par.settings = tactile.theme())


## classify: shallow (<50cm) | everything else
x$depth.class <- factor(x$depth.class, ordered = TRUE)
x$simple.depth.class <- ifelse(x$depth.class <= 'shallow', 'shallow', 'deep')

# mostly > 50cm
knitr::kable(
  prop.table(xtabs(~ source + depth.class, data = site(x)), margin = 1), 
  digits = 2
)

prop.table(xtabs(~ source + simple.depth.class, data = site(x)), margin = 1)

## develop dominant condition: simplified soil depth class
s <- site(x)
z <- split(s, s$mukey)
z <- lapply(z, dominantCondition, v = 'depth.to.restriction')

# develop LUT by mukey/cokey
depth.lut <- do.call('rbind', z)


## most-frequent soil texture class (<2mm) by dominant soil depth condition
# SPC
x.sub <- subset(x, cokey %in% unique(depth.lut$cokey)) 

# check: ok
par(mar = c(0, 0, 3, 1))
plotSPC(x.sub[1:30, ], color = 'texture')

plotSPC(x.sub, color = 'texture', name = '', print.id = FALSE, width = 0.35)

ragg::agg_png(filename = 'figures/coweeta-RSS-SSURGO-components-texture.png', width = 1200, height = 500)

par(mar = c(0, 0, 3, 0))
groupedProfilePlot(x.sub, groups = 'source', color = 'texture', name = '', print.id = FALSE, width = 0.35, depth.axis = list(style = 'compact', line = -6, cex = 0.8), col.label = 'Texture Class (<2mm fraction)', col.legend.cex = 1.5)

dev.off()


par(mar = c(0, 0, 3, 0))
groupedProfilePlot(x.sub, groups = 'source', color = 'texture', name = '', label = 'compname', width = 0.35, depth.axis = list(style = 'compact', line = -6, cex = 0.8), col.label = 'Texture Class (<2mm fraction)', col.legend.cex = 1.5)


##

x$pi <- profileInformationIndex(x, vars = c('awc_r'))
tapply(x$pi, x$source, median)

x.u <- unique(x, vars = c('hzdept_r', 'hzdepb_r', 'awc_r', 'compname', 'hzname'))
table(x.u$source)

par(mar = c(0, 0, 3, 0))
groupedProfilePlot(x.u, groups = 'source', color = 'awc_r', name = '', label = 'compname', width = 0.35, depth.axis = list(style = 'compact', line = -4, cex = 0.8), col.label = 'AWC', col.legend.cex = 1)




## TODO: is this reasonable?
## derive most frequent soil texture

## is this reasonable?
# --> consider 0-50cm
## depth-weighted mean SSC (to depth) -> soil texture

# wt. mean sand and clay to depth
a <- slab(x.sub, cokey ~ sandtotal_r + claytotal_r, slab.structure = c(0, 200), slab.fun = mean, na.rm = TRUE)

w <- dcast(a, cokey ~ variable, value.var = 'value')
w$texture <- ssc_to_texcl(sand = w$sandtotal_r, clay = w$claytotal_r, simplify = TRUE)

## check degree of aggregation
g <- merge(horizons(x.sub)[, c('cokey', 'texture')], w[, c('cokey', 'texture')], by = 'cokey', all.x = TRUE, sort = FALSE)

names(g) <- c('cokey', 'original.texture', 'agg.texture')

g$agg.texture <- factor(g$agg.texture, levels = levels(g$original.texture))


# cross-tabulation
# note that range of soil texture classes is reduced
g.tab <- table(original = g$original.texture, aggregated = g$agg.texture)
round(prop.table(g.tab, margin = 1) * 100)



## TODO: report on the loss of detail, esp. abrupt textural changes
# hmm.. a lot of simplification
plotSPC(x.sub[1:5, ], color = 'texture')
w[1:5, ]


## final LUT
final.lut <- merge(depth.lut, w, by = 'cokey', all.x = TRUE, sort = FALSE)

## create "soil type" codes: interaction between simple depth class * aggregate soil texture class
final.lut$soil.type <- interaction(final.lut$simple.depth.class, final.lut$texture)

# check
table(final.lut$source, final.lut$soil.type)


# save
saveRDS(final.lut, file = 'data/soil-depth-texture-classe-lut.rds')


