library(terra)
library(aqp)
library(igraph)
library(viridisLite)

## mukey
# RSS
r <- rast('grids/rss_utm.tif')
# SSURGO
s <- rast('grids/ssurgo_utm.tif')

## TODO: compare compname and many other things

## simplified soil type
# RSS
r <- rast('grids/rss-soiltype-class.tif')
# SSURGO
s <- rast('grids/ssurgo-soiltype-class.tif')


## combined mu/component/hz data as SPC
x <- readRDS('data/combined-tab-data-SPC.rds')

# mukind by source
mu <- unique(site(x)[, c('mukey', 'mukind', 'source')])
table(mu$source, mu$mukind)

# combine into 2 band raster
z <- c(r, s)

# cross-tabulation of RSS vs SSURGO MU
# by pixel
a <- crosstab(z)

# quick graphical look
a.q <- a
a.q[] <- ecdf(a)(a)
par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white')
image(a.q, col = mako(100), axes = FALSE, asp = 1)

## TODO: explore further with ordination
a

# generate long-formated version
e <- spatSample(z, size = 1000, method = 'regular')
e <- na.omit(e)

nrow(e)
head(e)

# re-name for clarity
names(e) <- c('rss', 'ssurgo')

# compute weights, as number of duplicate edges
e$id <- sprintf("%s-%s", e$rss, e$ssurgo)

# this is the edge list + weights
el <- as.data.frame(table(e$id))
.txt <- strsplit(as.character(el$Var1), '-', fixed = TRUE)

# re-format after tabulating unique edges
el$rss <- sapply(.txt, '[[', 1)
el$ssurgo <- sapply(.txt, '[[', 2)

# remove ID, re-name weight
el$Var1 <- NULL
names(el)[1] <- 'weight'

# re-arrange for igraph
el <- el[, c('ssurgo', 'rss', 'weight')]

# vertex metadata
v <- rbind(
  data.frame(
    name = unique(el$rss),
    source = 'rss',
    color = 2
  ),
  data.frame(
    name = unique(el$ssurgo),
    source = 'ssurgo',
    color = 3
  )
)

# init graph from long-formatted node adjacency
# also includes weights
# also includes vertex metadata
g <- graph_from_data_frame(el, directed = TRUE, vertices = v)

is.weighted(g)

V(g)$size <- sqrt(degree(g)) * 10

par(mar = c(0, 0, 0, 0))
plot(g, layout = layout_with_fr, edge.arrow.size = 0.5)

