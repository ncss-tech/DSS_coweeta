library(aqp)
library(lattice)
library(tactile)
library(reshape2)

source('local-functions.R')

## combined mu/component data, as SPC
x <- readRDS('data/combined-tab-data-SPC.rds')

## original proportions of depth class / by source
xtabs(~ source + depth.class, data = site(x))

histogram(~ depth | source, data = site(x), par.settings = tactile.theme())


## classify: shallow (<50cm) | everything else
x$depth.class <- factor(x$depth.class, ordered = TRUE)
x$simple.depth.class <- ifelse(x$depth.class <= 'shallow', 'shallow', 'deep')

# mostly > 50cm
prop.table(xtabs(~ source + simple.depth.class, data = site(x)), margin = 1)

## develop dominant condition: simplified soil depth class
s <- site(x)
z <- split(s, s$mukey)
z <- lapply(z, dominantCondition, v = 'simple.depth.class')

# develop LUT by mukey/cokey
depth.lut <- do.call('rbind', z)


## most-frequent soil texture class (<2mm) by dominant soil depth condition
# SPC
x.sub <- subset(x, cokey %in% unique(depth.lut$cokey)) 

# classify <2mm soil texture class
x.sub$texture <- ssc_to_texcl(sand = x.sub$sandtotal_r, clay = x.sub$claytotal_r, simplify = TRUE) 

# check: ok
par(mar = c(0, 0, 2, 1))
plotSPC(x.sub[1:30, ], color = 'texture')

## TODO: is this reasonable?
## derive most frequent soil texture

## is this reasonable?
# --> consider 0-50cm
## depth-weighted mean SSC (to depth) -> soil texture

# wt. mean sand and clay to depth
a <- slab(x.sub, cokey ~ sandtotal_r + claytotal_r, slab.structure = c(0, 200), slab.fun = mean, na.rm = TRUE)

w <- dcast(a, cokey ~ variable, value.var = 'value')
w$texture <- ssc_to_texcl(sand = w$sandtotal_r, clay = w$claytotal_r, simplify = TRUE)

# check

# note that range of soil texture classes is reduced
table(x.sub$texture)
table(w$texture)

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


