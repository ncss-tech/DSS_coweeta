library(clhs)
library(latticeExtra)
library(tactile)
library(hexbin)
library(reshape2)
library(viridisLite)
library(psych)


# full exploration of data space
# 1,000 data points
d <- data.frame(
  x1 = runif(1000, min = 0, max = 1),
  x2 = runif(1000, min = 5, max = 10),
  x3 = runif(1000, min = 2, max = 5),
  x4 = runif(1000, min = 0, max = 20),
  x5 = runif(1000, min = 0, max = 1),
  x6 = runif(1000, min = 0, max = 5),
  x7 = runif(1000, min = 0, max = 60)
)

# marginal distributions
m <- melt(d)
bwplot(variable ~ value, data = m, par.settings = tactile.theme())


# cLHS subset
# 100 optimally positioned points in 7D space
# 150x faster than it used to be, thanks to compiled code
s <- clhs(d, size = 100, simple = TRUE, use.cpp = TRUE)

# combine full dataset + subset
g <- make.groups(full = d, cLHS = d[s, ])

# ok
head(g)

# wide -> long format for plotting
m <- melt(g, id.vars = 'which')

# compare marginal distributions
bwplot(
  which ~ value | variable, 
  data = m, 
  par.settings = tactile.theme(), 
  scales = list(x = list(relation = 'free')), 
  as.table = TRUE
)


# pair-wise comparisons
hexplom(d, colramp = viridis, xbin = 8, trans = log, inv = exp)
hexplom(d[s, ], colramp = viridis, xbin = 8, trans = log, inv = exp)



## Will need to think about if this is reasonable...

# joint distributions via correlation
cor.d <- cor(d, method = 'pearson')
cor.s <- cor(d[s, ], method = 'pearson')

# yeah, need to read-up on this
cortest.normal(cor.d, cor.s, n1 = 1000, n2 = 100)
cortest.mat(cor.d, cor.s, n1 = 1000, n2 = 100)


