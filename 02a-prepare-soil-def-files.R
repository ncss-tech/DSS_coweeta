library(aqp)
library(soilDB)
library(lattice)
library(tactile)
library(reshape2)


source('local-functions.R')

## dominant component + map unit data
dominant.cokey.lut <- readRDS('data/dominant-cokey-LUT.rds')

## combined mu/component data, as SPC
x <- readRDS('data/combined-tab-data-SPC.rds')


## unit conversions

# notes: 
# * AWC [cm/cm] does not need to be converted
# * convert depths -> m later
# * Db still in g/cm^3 ?
# * kffact is character... ? 

# kffact oddity
x$kffact <- as.numeric(x$kffact)

# percent -> fraction
.v <- c('claytotal_r', 'sandtotal_r', 'silttotal_r', 'om_r', 'wsatiated_r', 'wthirdbar_r')

for(i in .v) {
  x[[i]] <- x[[i]] * 0.01
}

# ksat
# um/s -> m/day
x$ksat_r <- x$ksat_r * 1e-6 * 60 * 60 * 24

# log-transform for later
x$log_ksat <- log(x$ksat_r)
x$log_wsatiated <- log(x$wsatiated_r)

# horizon mid-points for later
x$mid <- (x$hzdept_r + x$hzdepb_r) / 2


## hz-thickness wt. mean, over all components
## simpler to do this all at once
## 0-200cm depth interval considered
a <- slab(
  x, 
  fm = cokey ~ sandtotal_r + silttotal_r + claytotal_r + kffact + dbovendry_r + partdensity + awc_r + wthirdbar_r + wsatiated_r + om_r + ksat_r, 
  slab.fun = mean, 
  na.rm = TRUE,
  slab.structure = c(0, 200)
)

# long -> wide
hz.wt.mean <- dcast(a, cokey ~ variable, value.var = 'value')

## iteration over components
decayFunctions <- function(i) {
  
  # estimate exp decay functions
  h <- horizons(i)
  
  # this may fail, if sufficient data are missing
  # model fit may be good or bad... we just don't know!
  
  # log(x) ~ hz mid-point
  m.ksat <- try(lm(log_ksat ~ mid, data = h), silent = TRUE)
  m.por <- try(lm(log_wsatiated ~ mid, data = h), silent = TRUE)
  
  if(inherits(m.ksat, 'try-error')) {
    # NA place-holder coefs for now
    ed.ksat <- c(NA, NA)
  } else {
    ed.ksat <- coef(m.ksat)
  }
  
  if(inherits(m.por, 'try-error')) {
    # NA place-holder coefs for now
    ed.por <- c(NA, NA)
  } else {
    ed.por <- coef(m.por)
  }
  
  # compile results
  .res <- data.frame(
    cokey = h$cokey[1],
    ksat_0 = exp(ed.ksat[1]),
    ksat_decay = -ed.ksat[2],
    por_0 = exp(ed.por[1]),
    por_decay = -ed.por[2]
  )
  
  row.names(.res) <- NULL
  
  # keep subset
  return(.res)
}

df <- profileApply(x, decayFunctions, simplify = FALSE, frameify = TRUE)

## soil depth
# convert to m
x$soil.depth <- getSoilDepthClass(x)$depth * 0.01 

## combine pieces
z <- merge(hz.wt.mean, df, by = 'cokey', all.x = TRUE, sort = FALSE)

## combine with mukey, comppct, soil depth
z <- merge(z, site(x)[, c('mukey', 'cokey', 'comppct_r', 'soil.depth')], by = 'cokey', all.x = TRUE, sort = FALSE)


## aggregate or subset for 1 row / mukey

## largest component
z.sub <- z[which(z$cokey %in% dominant.cokey.lut$cokey), ]


## fill missing / obviously wrong values with plausible estimates
##  * regional mean ?
##  * defaults ? 

# particle density, use density of quartz
z.sub$partdensity <- 2.6

# ksat decay function
# use regional mean
z.sub$ksat_0[is.na(z.sub$ksat_0)] <- mean(z.sub$ksat_0, na.rm = TRUE)
z.sub$ksat_decay[is.na(z.sub$ksat_decay)] <- mean(z.sub$ksat_decay, na.rm = TRUE)

# porosity decay function
z.sub$por_0[is.na(z.sub$por_0)] <- mean(z.sub$por_0, na.rm = TRUE)
z.sub$por_decay[is.na(z.sub$por_decay)] <- mean(z.sub$por_decay, na.rm = TRUE)


## interpret soil texture class of fine earth fraction (<2mm)
# this is is a rough approximation of *entire soil profiles* ...
z.sub$texture <- ssc_to_texcl(sand = z.sub$sandtotal_r * 100, clay = z.sub$claytotal_r * 100)


## final checks
summary(z.sub)

head(z.sub)

knitr::kable(
  z.sub, 
  row.names = FALSE, digits = 3
)

knitr::kable(
  z.sub[z.sub$mukey == '3244759', ], 
  row.names = FALSE, digits = 3
)


## save tabular output
saveRDS(z.sub, file = 'data/soil-definitions-by-mukey.rds')


## craft soil def files


