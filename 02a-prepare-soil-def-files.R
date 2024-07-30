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

## consider using ROSETTA for estimation of hydraulic properties / VG parameters


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
  fm = cokey ~ sandtotal_r + silttotal_r + claytotal_r + kffact + dbovendry_r + partdensity + awc_r + wthirdbar_r + wsatiated_r + om_r + ksat_r + log_ksat, 
  slab.fun = mean, 
  na.rm = TRUE,
  slab.structure = c(0, 200)
)

# long -> wide
hz.wt.mean <- dcast(a, cokey ~ variable, value.var = 'value')


## these aren't likely reliable, probably better to try this over a much larger group of related data

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


## 2024-07-30: use constant decay coef.
z$por_decay <- 4000
z$ksat_decay <- 0.12


## back-transform wt mean log(ksat)
# plot(exp(log_ksat) ~ ksat_r, data = z, las = 1)
# abline(0, 1)

# using wt. geometric mean of Ksat due to highly skewed distribution 
z$ksat_r <- exp(z$log_ksat)


## Ksat_0 missing values
# use regional mean
z$ksat_0[which(is.na(z$ksat_0))] <- mean(z$ksat_0, na.rm = TRUE)

## porosity_0 missing values
# use regional mean
z$por_0[which(is.na(z$por_0))] <- mean(z$por_0, na.rm = TRUE)


## particle density, use density of quartz
z$partdensity <- 2.6

## interpret soil texture class of fine earth fraction (<2mm)
# this is is a rough approximation of *entire soil profiles* ...
z$texture <- ssc_to_texcl(sand = z$sandtotal_r * 100, clay = z$claytotal_r * 100)


## aggregate or subset for 1 row / mukey

## largest component
z.sub <- z[which(z$cokey %in% dominant.cokey.lut$cokey), ]


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
saveRDS(z, file = 'data/soil-definitions-by-cokey.rds')
saveRDS(z.sub, file = 'data/soil-definitions-by-mukey.rds')


## craft soil def files

# iterate over rows, converting to named list
pn <- lapply(
  split(z.sub, 1:nrow(z.sub)), 
  toParameterNames
)

# write each named list to soil parameter file, by mukey
.trash <- lapply(pn, function(i) {
  .f <- sprintf('soil-parameter-files/by-mukey/soil_%s.def', i$patch_default_ID)
  writeSoilDefinitionFile(i, f = .f)
})




