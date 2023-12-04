## TODO:
## * soil parameter files for each map unit key (dominant component)
## * weighted avg. soil properties to contact / rest. feature
## * use ROSETTA for Ksat and water retention curve points

library(aqp)
library(soilDB)
library(lattice)
library(tactile)
library(reshape2)


source('local-functions.R')

## combined mu/component data, as SPC
x <- readRDS('data/combined-tab-data-SPC.rds')


## develop ROSETTA estimates
# https://ncss-tech.github.io/AQP/soilDB/ROSETTA-API.html
.v <-c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_r', 'wfifteenbar_r')
.r <- horizons(x)[, c('hzID', .v)]
R <- ROSETTA(.r, vars = .v, v = '3')

# check: ok
# head(R)

# splice-into horizons
horizons(x) <- R[, c('hzID', 'theta_r', 'theta_s', 'alpha', 'npar', 'ksat')]


## investigate mukind by source
xtabs(~ source + mukind, data = unique(site(x)[, c('mukey', 'source', 'mukind')]))

## restrictive features
r <- diagnostic_hz(x)

# kind
(.tab <- xtabs( ~ reskind + which, data = r))
round(prop.table(.tab, margin = 2), 2)




## a very SIMPLE selection of 1 component / map unit
## based on most frequent component name ~ sum(component percent)
s <- site(x)
ss <- split(s, s$mukey)
z <- lapply(ss, dominantComponent)
dominant.cokey.lut <- do.call('rbind', z)
row.names(dominant.cokey.lut) <- NULL

# check number / source
table(dominant.cokey.lut$source)
table(dominant.cokey.lut$source, dominant.cokey.lut$majcompflag)

# ensure no duplicate map unit keys
stopifnot(! any(table(dominant.cokey.lut$mukey) > 1))

# check for missing mukeys 
setdiff(unique(x$mukey), dominant.cokey.lut$mukey)
setdiff(dominant.cokey.lut$mukey, unique(x$mukey))

# save
saveRDS(dominant.cokey.lut, file = 'data/dominant-cokey-LUT.rds')




## weighted mean depth to restrictive features
z <- lapply(ss, wtMeanProperty, v = 'depth.to.restriction')
rest.depth.wt.mean <- do.call('rbind', z)
row.names(rest.depth.wt.mean) <- NULL

# save
saveRDS(rest.depth.wt.mean, file = 'data/rest-depth-wtmean-LUT.rds')



## wt. mean soil properties to contact


# convert log10(cm/day) -> cm/day
x$ksat.rosetta <- 10^x$ksat
# convert um/s -> cm/day
x$ksat_r <- x$ksat_r * 8.64

# truncate each profile, depth to restriction
y <- trunc(x, 0, x$depth.to.restriction)


# figure out proper weighting approach
a.site <- slab(y, fm = source ~ sandtotal_r + silttotal_r + claytotal_r + dbthirdbar_r + wthirdbar_r + wfifteenbar_r + ksat_r + ksat.rosetta)

a.site$source <- factor(a.site$source)

levels(a.site$variable) <- c('Sand (%)', 'Silt (%)', 'Clay (%)', 'Db 1/3 bar (g/cm^3)', 'Water Retention 1/3 bar (%)',  'Water Retention 15 bar (%)', 'Ksat [SSURGO] (cm/d)', 'Ksat [ROSETTA] (cm/d)')

# define plotting style
tps <- tactile.theme(superpose.line = list(col = hcl.colors(2, palette = 'roma'), lwd = 2))

# plot grouped, aggregate data
xyplot(top ~ p.q50 | variable, groups = source, data=a.site, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=a.site$p.q25, upper=a.site$p.q75, ylim=c(205,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=a.site$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(4,2), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       par.settings=tps,
       as.table = TRUE,
       auto.key=list(columns=2, lines=TRUE, points=FALSE)
)


# ^^ note influence of O horizons, which seem to be present in RSS vs. SSURGO



##


# truncate each profile, depth to restriction
y <- trunc(x, 0, x$depth.to.restriction)

a.site <- slab(y, fm = mukey ~ sandtotal_r + silttotal_r + claytotal_r + dbthirdbar_r + wthirdbar_r + wfifteenbar_r + ksat_r + ksat, weights = 'comppct_r')

a.site$mukey <- factor(a.site$mukey)

levels(a.site$variable) <- c('Sand (%)', 'Silt (%)', 'Clay (%)', 'Db 1/3 bar (g/cm^3)', 'Water Retention 1/3 bar (%)',  'Water Retention 15 bar (%)', 'Ksat (SSURGO)', 'Ksat (ROSETTA)')

# define plotting style
tps <- tactile.theme(superpose.line = list(col = c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd = 2))

# plot grouped, aggregate data
xyplot(top ~ p.q50 | variable, data = a.site, 
       subset = mukey == '545842',
       ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=a.site$p.q25, upper=a.site$p.q75, ylim=c(155,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=a.site$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(4,2), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       par.settings=tps,
       as.table = TRUE,
       auto.key=list(columns=2, lines=TRUE, points=FALSE)
)




## TODO: finish wt. mean over depth-to-restrictive features

a <- slab(y, fm = cokey ~ sandtotal_r + silttotal_r + claytotal_r + dbthirdbar_r + wthirdbar_r + wfifteenbar_r + ksat_r, slab.fun = mean, na.rm = TRUE)


# long -> wide format
a.wtmean <- dcast(a, cokey ~ variable, fun.aggregate = mean, na.rm = TRUE)
head(a.wtmean)
hist(a$sandtotal_r)


# save


## aggregate to mukey 




par(mar = c(0, 1, 3, 2))
plotSPC(x, color = 'ksat', print.id = FALSE, name = NA, lwd = 0)





plotSPC(y, color = 'claytotal_r', print.id = FALSE, name = NA, lwd = 0)






########################### old stuff, pending integration ##############################





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

x$pi <- profileInformationIndex(x, vars = c('awc_r'), method = 'joint', baseline = FALSE, compression = 'gzip')
tapply(x$pi, x$source, median)

bwplot(source ~ pi, data = site(x))


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


