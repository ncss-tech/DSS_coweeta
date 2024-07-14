library(aqp)
library(soilDB)
library(lattice)
library(tactile)
library(reshape2)


source('local-functions.R')

## dominant component + map unit data
dominant.cokey.lut <- readRDS('data/dominant-cokey-LUT.rds')

## combined mu/component data, as SPC
x <- readRDS('data/combined-tab-data-SPC.rds')


## TODO: why is kffact not numeric?
x$kffact <- as.numeric(x$kffact)


z <- subset(x, mukey == '3244758')

z$shortName <- sprintf("%s %s%%", z$compname, z$comppct_r)

par(mar = c(0, 0, 3, 2))
plotSPC(z, color = 'claytotal_r', label = 'shortName', cex.names = 0.8)
mtext(text = 'mukey 3244758', side = 2, line = -2)


a <- slab(
  z, fm = mukey ~ sandtotal_r + silttotal_r + claytotal_r + kffact + dbovendry_r + partdensity + awc_r + wthirdbar_r + wsatiated_r + om_r, 
  slab.fun = mean, 
  na.rm = TRUE
)

b <- slab(
  z, fm = mukey ~ sandtotal_r + silttotal_r + claytotal_r + kffact + dbovendry_r + partdensity + awc_r + wthirdbar_r + wsatiated_r + om_r, 
  weights = 'comppct_r',
  slab.fun = weighted.mean, 
  na.rm = TRUE
)

d <- slab(
  subset(z, cokey == '3244758:2807870'), 
  fm = mukey ~ sandtotal_r + silttotal_r + claytotal_r + kffact + dbovendry_r + partdensity + awc_r + wthirdbar_r + wsatiated_r + om_r, 
  slab.fun = mean, 
  na.rm = TRUE
)


g <- make.groups(mean = a, weighted.mean = b, dominant.component = d)
g$which <- factor(g$which, labels = c('Mean\nAll Components', 'Weighted Mean\nAll Components', 'Largest Component'))


xyplot(
  top ~ value | variable,
  groups = which,
  data = g,
  ylim = c(200, -10), 
  as.table = TRUE,
  scales = list(x = list(relation = 'free'), y = list(alternating = 3)),
  xlab = 'Wt. Mean',
  ylab = 'Depth (cm)',
  panel = panel.depth_function,
  prepanel = prepanel.depth_function,
  strip = strip.custom(bg = grey(0.85)),
  par.settings = tactile.theme(superpose.line = list(lwd = 2)),
  auto.key = list(columns = 3, lines = TRUE, points = FALSE)
)


xyplot(
  top ~ value | variable,
  groups = which,
  data = g,
  subset = variable %in% c('sandtotal_r', 'silttotal_r', 'claytotal_r'),
  ylim = c(200, -10), 
  as.table = TRUE,
  scales = list(x = list(relation = 'free'), y = list(alternating = 3)),
  xlab = 'Wt. Mean',
  ylab = 'Depth (cm)',
  panel = panel.depth_function,
  prepanel = prepanel.depth_function,
  strip = strip.custom(bg = grey(0.85)),
  par.settings = tactile.theme(superpose.line = list(lwd = 2)),
  auto.key = list(columns = 3, lines = TRUE, points = FALSE)
)





a <- slab(
  z, fm = mukey ~ sandtotal_r + silttotal_r + claytotal_r + kffact + dbovendry_r + partdensity + awc_r + wthirdbar_r + wsatiated_r + om_r, 
  slab.fun = mean, 
  na.rm = TRUE, 
  slab.structure = c(0, 150)
)

b <- slab(
  z, fm = mukey ~ sandtotal_r + silttotal_r + claytotal_r + kffact + dbovendry_r + partdensity + awc_r + wthirdbar_r + wsatiated_r + om_r, 
  weights = 'comppct_r',
  slab.fun = weighted.mean, 
  na.rm = TRUE,
  slab.structure = c(0, 150)
)

d <- slab(
  subset(z, cokey == '3244758:2807870'), 
  fm = mukey ~ sandtotal_r + silttotal_r + claytotal_r + kffact + dbovendry_r + partdensity + awc_r + wthirdbar_r + wsatiated_r + om_r, 
  slab.fun = mean, 
  na.rm = TRUE,
  slab.structure = c(0, 150)
)


g <- make.groups(mean = a, weighted.mean = b, dominant.component = d)


w <- dcast(g, mukey + which ~ variable, value.var = 'value')

knitr::kable(w, row.names = FALSE, digits = 2)

