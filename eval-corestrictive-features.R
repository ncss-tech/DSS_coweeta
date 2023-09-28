library(aqp)

## combined mu/component data, as SPC
x <- readRDS('data/combined-tab-data-SPC.rds')


x.sub <- subset(x, source == 'SSURGO')[1:20, ]

par(mar = c(0, 0, 3, 1))
plotSPC(x.sub, label = 'compname', name.style = 'center-center', width = 0.35, depth.axis = list(style = 'compact', line = -3, cex = 0.8), col.label = 'Texture Class (<2mm fraction)', color = 'texture')


par(mar = c(0, 0, 3, 1))
plotSPC(x.sub, label = 'compname', id.style = 'top', cex.id = 0.66, cex.names = 0.85, name.style = 'center-center', width = 0.35, depth.axis = list(style = 'compact', line = -3, cex = 0.8), col.label = 'Texture Class (<2mm fraction)', color = 'texture', col.palette = hcl.colors(10, 'vik', rev = TRUE))

addDiagnosticBracket(x.sub, kind = 'Lithic bedrock', feature = 'reskind', top = 'resdept_r',  bottom = 'resdepb_r', offset = -0.5, col = 2, tick.length = 0, lwd = 4)

addDiagnosticBracket(x.sub, kind = 'Paralithic bedrock', feature = 'reskind', top = 'resdept_r',  bottom = 'resdepb_r', offset = -0.5, col = 3, tick.length = 0, lwd = 4)

addDiagnosticBracket(x.sub, kind = 'Strongly contrasting textural stratification', feature = 'reskind', top = 'resdept_r',  bottom = 'resdepb_r', offset = -0.5, col = 4, tick.length = 0, lwd = 4)

legend('topright', legend = c('lithic contact', 'paralithic', 'strongly contrasting textural change'), cex = 0.8, bty = 'n', lwd = 3, col = 2:4, xpd = NA, inset = c(0, -0.06))

