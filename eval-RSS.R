library(terra)
library(aqp)

z <- readRDS('Raster soil survey/Mesic_results/training_data.rds')
str(z)

table(z$Current_Ta)

# (x <- rast('Raster soil survey/Frigid_results/probmatrix_sffrigid.tif'))
(x <- rast('Raster soil survey/Mesic_results/probmatrix_sf2.tif'))

plot(x)

hist(x / 100)

# plot(x / 100)

v <- values(x / 100)
h <- x[[1]]
names(h) <- 'Shannon H'

values(h) <- apply(v, 1, shannonEntropy, b = 2)
h[h == 0] <- NA

equal.prob.H <- function(n, b) {
  p <- rep(1, times = n) / n
  shannonEntropy(p, b = b)
}

hist(h, breaks = 50)
abline(v = equal.prob.H(nlyr(x), b = 2), col = 2)
abline(v = equal.prob.H(nlyr(x), b = 2) * 0.95, col = 2)
abline(v = equal.prob.H(nlyr(x), b = 2) * 0.90, col = 2)

## what is a reasonable threshold for "no information rate"?

equal.prob.H(nlyr(x), b = 2)

plot(h)
plot(h / equal.prob.H(n = nlyr(x), b = 2))

plot(h > equal.prob.H(n = nlyr(x), b = 2) * 0.95)
plot(h > equal.prob.H(n = nlyr(x), b = 2) * 0.90)
plot(h > equal.prob.H(n = nlyr(x), b = 2) * 0.85)
plot(h > equal.prob.H(n = nlyr(x), b = 2) * 0.80)



idx <- which(values(h) > equal.prob.H(nlyr(x)) * 0.8)

v[idx[1:10], ]
apply(v[idx[1:10], ], 1, shannonEntropy)


hist(h / equal.prob.H(n = nlyr(x), b = 2), las = 1)







