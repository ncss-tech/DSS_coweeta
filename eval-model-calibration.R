library(lattice)
library(tactile)

s <- read.table('calibration-eval/calibrationoutputssurgo.txt', header = TRUE)
d <- read.table('calibration-eval/calibrationoutputdsm.txt', header = TRUE)

x <- make.groups(SSURGO = s, `Raster Soil Survey` = d)

## maybe not all that informative as compared with the distributions themselves
# wilcox.test(lnNSE ~ which, data = x)

# interesting
bwplot(which ~ NSE, data = x, par.settings = tactile.theme(), notch = TRUE)

bwplot(which ~ lnNSE, data = x, par.settings = tactile.theme(), notch = TRUE)
bwplot(which ~ KGE, data = x, par.settings = tactile.theme(), notch = TRUE)

bwplot(which ~ smNSE, data = x, par.settings = tactile.theme(), notch = TRUE)
bwplot(which ~ smKGE, data = x, par.settings = tactile.theme(), notch = TRUE)


