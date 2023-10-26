library(lattice)
library(latticeExtra)
library(tactile)

ssurgo <- read.table('calibration-eval/ssurgocalibrationoutput1023.txt', header = TRUE)
rss <- read.table('calibration-eval/dsmcalibrationoutput1023.txt', header = TRUE)
static <- read.table('calibration-eval/staticcalibrationoutput1023.txt', header = TRUE)

x <- make.groups(
  SSURGO = ssurgo, 
  `Raster Soil Survey` = rss,
  `Static Soil Data` = static
)

## maybe not all that informative as compared with the distributions themselves
# wilcox.test(lnNSE ~ which, data = x)

# interesting
bwplot(which ~ valNSE, data = x, par.settings = tactile.theme(), notch = TRUE, scales = list(x = list(tick.number = 10)))

bwplot(which ~ vallnNSE, data = x, par.settings = tactile.theme(), notch = TRUE, scales = list(x = list(tick.number = 10)))

bwplot(which ~ valKGE, data = x, par.settings = tactile.theme(), notch = TRUE, scales = list(x = list(tick.number = 10)))

bwplot(which ~ valsmNSE, data = x, par.settings = tactile.theme(), notch = TRUE, scales = list(x = list(tick.number = 10)))



p1 <- bwplot(which ~ valKGE, 
       data = x, 
       main = 'Streamflow Predictions',
       xlab = 'KGE (validation)',
       par.settings = tactile.theme(), 
       notch = TRUE, 
       scales = list(x = list(tick.number = 15)), 
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
       scales = list(x = list(tick.number = 15)), 
       panel = function(...) {
         panel.grid(-1, -1)
         panel.abline(v = -0.41, lty = 2, col = 2, lwd = 2)
         panel.bwplot(...)
       }
)

print(p1, split = c(1, 1, 1, 2), more = TRUE)
print(p2, split = c(1, 2, 1, 2), more = FALSE)



