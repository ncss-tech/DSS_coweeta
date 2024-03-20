library(soilDB)
library(sharpshootR)
library(hydromad)
library(terra)


## see / adapt leaky-bucket-via-ISSR800-AWC-CONUS.R


# GCS
ppt <- rast('e:/gis_data/prism/final_monthly_ppt_800m.tif')
pet <- rast('e:/gis_data/prism/final_monthly_pet_800m.tif')

# UTM
elev <- rast('grids/elev_pcs.tif')
aws <- rast('grids/archive/rss_aws050.tif')

# watershed
w <- vect('vect/Coweeta_Hydrologic_Laboratory.shp')

# convert AWS from cm -> mm
aws <- aws * 10

# warp / resample / crop to elevation
ppt <- project(ppt, elev, method = 'bilinear')
pet <- project(pet, elev, method = 'bilinear')

# fix raster layer names
.months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
names(ppt) <- sprintf("pet%s", .months)
names(ppt) <- sprintf("ppt%s", .months)
names(elev) <- 'elev'
names(aws) <- 'awc'

# combine all data into single collection
x <- c(ppt, pet, elev, aws)

# try at coarser resolution
a <- aggregate(x, fact = 10)
a <- mask(crop(a, a[[26]]), a[[26]])

names(a)[1:12]
names(a)[13:24]

# rs <- unlist(a[30, 35])

runWB <- function(rs) {
  
  # check for AWS
  if(!is.na(rs[26])) {
    
    .wb <- try(
      sharpshootR::monthlyWB(
        AWC = rs[26],
        PPT = rs[1:12], PET = rs[13:24], 
        rep = 3, 
        keep_last = TRUE
      ),
      silent = TRUE
    )
    
    if(inherits(.wb, 'try-error')) {
      print(rs)
      return(NA_real_)
    } else {
      
      D <- sum(.wb$D, na.rm = TRUE)
      U <- sum(.wb$U, na.rm = TRUE)
      AET <- sum(.wb$ET, na.rm = TRUE)
      AET_PET <- AET / sum(.wb$PET, na.rm = TRUE)
      
      .res <- c(sumD = D, sumU = U, AET = AET, AET_PET = AET_PET) 
      
      return(.res)
    }
    
    
    
  } else {
    return(rep(NA_real_, times = 4))
  }
}

# ~ 3.6 minutes
system.time(wb <- app(x, fun = runWB, cores = 8))

# check: reasonable

plot(wb, col = hcl.colors(25, palette = 'mako'), axes = FALSE, mar = c(1, 1, 2, 5))

plot(wb[[4]], col = hcl.colors(25, palette = 'mako'), axes = FALSE, mar = c(1, 1, 2, 5))


summary(aws)



s <- spatSample(x, size = 1, na.rm = TRUE, as.df = TRUE)


mwb1 <- monthlyWB(
  AWC = unlist(s[, 26]),
  PPT = unlist(s[, 1:12]), 
  PET = unlist(s[, 13:24]), 
  rep = 3, 
  keep_last = TRUE,
  distribute = FALSE
)

mwb2 <- monthlyWB(
  AWC = unlist(s[, 26]),
  PPT = unlist(s[, 1:12]), 
  PET = unlist(s[, 13:24]), 
  rep = 3, 
  keep_last = TRUE,
  distribute = TRUE
)


par(mfrow = c(2, 1))
plotWB(mwb1)
plotWB(mwb2)





