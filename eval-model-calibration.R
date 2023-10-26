library(lattice)
library(latticeExtra)
library(tactile)

# latest model runs + metrics c/o Carlos

ssurgo <- read.table('calibration-eval/ssurgocalibrationoutput1023.txt', header = TRUE)
rss <- read.table('calibration-eval/dsmcalibrationoutput1023.txt', header = TRUE)
static <- read.table('calibration-eval/staticcalibrationoutput1023.txt', header = TRUE)

x <- make.groups(
  `Static Soil Data` = static,
  SSURGO = ssurgo, 
  `Raster Soil Survey` = rss
)

## maybe not all that informative as compared with the distributions themselves
# wilcox.test(lnNSE ~ which, data = x)

# interesting
bwplot(which ~ valNSE, data = x, par.settings = tactile.theme(), notch = TRUE, scales = list(x = list(tick.number = 10)))

bwplot(which ~ vallnNSE, data = x, par.settings = tactile.theme(), notch = TRUE, scales = list(x = list(tick.number = 10)))

bwplot(which ~ valKGE, data = x, par.settings = tactile.theme(), notch = TRUE, scales = list(x = list(tick.number = 10)))

bwplot(which ~ valsmNSE, data = x, par.settings = tactile.theme(), notch = TRUE, scales = list(x = list(tick.number = 10)))


# add KGE mean equivalency threshold
# https://hess.copernicus.org/articles/23/4323/2019/

p1 <- bwplot(which ~ valKGE, 
             data = x, 
             main = 'Streamflow Predictions',
             xlab = 'KGE (validation)',
             par.settings = tactile.theme(), 
             notch = TRUE, 
             scales = list(x = list(tick.number = 10)), 
             panel = function(...) {
               panel.grid(-1, -1)
               panel.abline(v = -0.41, lty = 2, col = 2, lwd = 2)
               panel.bwplot(...)
             }
)

p2 <- bwplot(which ~ valsmKGE, 
             data = x, 
             main = 'Soil Moisture Predictions',
             xlab = 'KGE (validation)',
             par.settings = tactile.theme(), 
             notch = TRUE, 
             scales = list(x = list(tick.number = 10)), 
             panel = function(...) {
               panel.grid(-1, -1)
               panel.abline(v = -0.41, lty = 2, col = 2, lwd = 2)
               panel.bwplot(...)
             }
)

ragg::agg_png(filename = 'figures/Oct23-model-performance.png', width = 1200, height = 800, scaling = 2)

print(p1, split = c(1, 1, 1, 2), more = TRUE)
print(p2, split = c(1, 2, 1, 2), more = FALSE)

dev.off()


