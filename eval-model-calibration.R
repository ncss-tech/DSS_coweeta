library(lattice)
library(tactile)

s <- read.table('calibration-eval/calibrationoutputssurgo.txt', header = TRUE)
d <- read.table('calibration-eval/calibrationoutputdsm.txt', header = TRUE)

x <- make.groups(ssurgo = s, rss = d)

wilcox.test(s$lnNSE, d$lnNSE)

bwplot(which ~ lnNSE, data = x, par.settings = tactile.theme(), notch = TRUE)
bwplot(which ~ KGE, data = x, par.settings = tactile.theme(), notch = TRUE)

bwplot(which ~ smNSE, data = x, par.settings = tactile.theme(), notch = TRUE)
bwplot(which ~ smKGE, data = x, par.settings = tactile.theme(), notch = TRUE)


