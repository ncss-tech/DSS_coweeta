


d <- data.frame(
  z = c(0, 5, 25, 100, 150),
  Ksat = c(100, 65, 55, 20, 5)
)

plot(Ksat ~ z, data = d, type = 'b', las = 1)


n <- nls(Ksat ~ SSasymp(z, Asym, R0, lrc), data = d)
coef(n)

p <- predict(n, newdata = data.frame(z = 1:150))
lines(1:150, p, col = 2)


.f <- function(z, Ksat_0, r) {
  Ksat_0 * exp(-r * z)
}

plot(1:150, .f(1:150, Ksat_0 = 80, r = 0.05), las = 1)


d <- data.frame(
  z = seq(0, 150, length.out = 25)
)

d$ksat <- pmax(.f(d$z, Ksat_0 = 80, r = 0.05) + rnorm(n = 25, mean = 0, sd = 3), 0.01)

plot(ksat ~ z, data = d, type = 'b', las = 1)


d$logksat <- log(d$ksat)
plot(logksat ~ z, data = d, type = 'p', las = 1, cex = 0.5, pch = 16)

l <- lm(logksat ~ z, data = d)
summary(l)
coef(l)
exp(coef(l)[1])


plot(ksat ~ z, data = d, type = 'b', las = 1)
p <- exp(coef(l)[1]) * exp(coef(l)[2] * d$z)
lines(d$z, p, col = 2)


nls(ksat ~ SSasymp(z, Asym, R0, lrc), data = d)
pp <- predict(n, newdata = data.frame(z = d$z))
lines(d$z, pp, col = 3)






d <- dice(x[5, ], ~ ksat.rosetta, SPC = FALSE)
plot(ksat.rosetta ~ hzdept_r, data = d, type = 'p', las = 1, cex = 0.5, pch = 16)

## doesn't work
# n <- nls(ksat.rosetta ~ SSasymp(hzdept_r, Asym, R0, lrc), data = d)
# coef(n)
# p <- predict(n, newdata = data.frame(hzdept_r = 1:150))
# 
# lines(1:150, p, col = 2)

d$logksat <- log(d$ksat.rosetta)
plot(logksat ~ hzdept_r, data = d, type = 'p', las = 1, cex = 0.5, pch = 16)

l <- lm(logksat ~ hzdept_r, data = d)
summary(l)
coef(l)
