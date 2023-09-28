library(aqp)

## combined mu/component data, as SPC
x <- readRDS('data/combined-tab-data-SPC.rds')


s <- site(x)
xtabs( ~ source + compname, data = s, subset = majcompflag == 'Yes')


s.ssurgo <- subset(s, source == 'SSURGO')
s.rss <- subset(s, source == 'RSS')

setdiff(s.ssurgo$compname, s.rss$compname)
setdiff(s.rss$compname, s.ssurgo$compname)
