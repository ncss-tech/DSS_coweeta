


d <- data.frame(
  z = c(0, 5, 25, 100, 150),
  Ksat = c(100, 65, 55, 20, 5)
)

plot(Ksat ~ z, data = d, type = 'b', las = 1)


n <- nls(Ksat ~ SSasymp(z, Asym, R0, lrc), data = d)
coef(n)

p <- predict(n, newdata = data.frame(z = 1:150))
lines(1:150, p, col = 2)


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

plot(ksat.rosetta ~ hzdept_r, data = d, type = 'p', las = 1, cex = 0.5, pch = 16)

p <- exp(coef(l)[1]) * exp(-coef(l)[2] * 1:200)
lines(1:200, p, col = p)
