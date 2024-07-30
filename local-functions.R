##
##
##

# ## TODO: 
# #  * constant values?
# #  * not enough values to fit a curve? dice() -> nls() | optim() -> coef()
# #  * are exponential fits reasonable?
# #  * lm(log(y) ~ x)
# #  * ... group over larger collections of profiles
# 
# fitDecayFunction <- function(z, p0, p) {
#   
#   # solve for p
#   # res <- p0 * exp(-(z/p))
#   
#   return(res)
#   
# }


# wt. mean component level property
# i: map unit / component records, split by mukey
wtMeanProperty <- function(i, v) {
  
  # filter misc. areas
  .keep <- which(i$compkind != 'Miscellaneous area' & !is.na(i$comppct_r))
  i <- i[.keep, ]
  if(nrow(i) < 1) {
    return(NULL)
  }
  
  # wt. mean
  .wm <- weighted.mean(x = i[[v]], w = i$comppct_r, na.rm = TRUE)
  
  # assemble results
  .res <- data.frame(
    mukey = i$mukey[1],
    .v = .wm
  )
  
  # fix names
  names(.res)[2] <- v
  
  return(.res)
}


# dominant component within a single map unit
# i: map unit / component records, split by mukey
dominantComponent <- function(i) {
  
  # filter misc. areas
  .keep <- which(i$compkind != 'Miscellaneous area' & !is.na(i$comppct_r))
  i <- i[.keep, ]
  if(nrow(i) < 1) {
    return(NULL)
  }
  
  # largest component
  idx <- order(i$comppct_r, decreasing = TRUE)[1]
  
  return(i[idx, ])
}


# dominant condition by map unit key
dominantCondition <- function(i, v) {
  
  # filter misc. areas
  .keep <- which(i$compkind != 'Miscellaneous area' & !is.na(i$comppct_r))
  i <- i[.keep, ]
  if(nrow(i) < 1) {
    return(NULL)
  }
  
  # sum component percent by 'v'
  fm <- as.formula(sprintf("comppct_r ~ %s", v))
  a <- aggregate(fm, data = i, FUN = sum, na.rm = TRUE)
  
  # most frequent
  idx <- order(a[['comppct_r']], decreasing = TRUE)[1]
  
  # retain most frequent class and associated IDs
  res <- data.frame(
    mukey = i$mukey[1],
    source = i$source[1],
    v = a[[v]][idx],
    pct = a[['comppct_r']][idx]
  )
  
  # fix names
  names(res) <- c('mukey', 'source', v, 'pct')
  
  return(res)
}


# dominant value by map unit key, with associated component percent
dominantValue <- function(i, v) {
  
  i <- i[which(i$compkind != 'Miscellaneous area'), ]
  if(nrow(i) < 1) {
    return(NULL)
  }
  
  
  idx <- order(i[['comppct_r']], decreasing = TRUE)[1]
  
  res <- data.frame(
    mukey = i$mukey[1],
    v = i[[v]][idx],
    pct = i[['comppct_r']][idx]
  )
  
  names(res) <- c('mukey', v, 'pct')
  
  return(res)
  
}




## not currently using this 

#' @title Build a soil parameter list from SSURGO/RSS component data.
#'
#' @param s a `SoilProfileCollection` object
#' @param id 
#' @param template 
#'
#' @return list
#' @export
#'
#' @examples
buildParameterList <- function(s, template = NULL) {
  
  # create a bare-bones parameter list
  if(is.null(template)) {
    p <- list()
  } else {
    # start with the template
    p <- template
  }
  
  ##
  ## Estimation of parameters via aggregation
  ##
  
  ## TODO: decide on what to do with organic horizons, which could be missing data
  # remove organic horizons
  s <- subsetHz(s, ! grepl('O', hzDesgn(s)))
  
  
  # soil depth
  .soildepth <- estimateSoilDepth(s)
  
  # aggregate over entire soil depth, or specific depth interval
  a <- suppressMessages(
    slab(s, fm = ~ sandtotal_r + silttotal_r + claytotal_r, slab.structure = c(0, .soildepth), strict = FALSE, slab.fun = mean, na.rm = TRUE)
  )
  
  # long -> wide
  a.wide <- reshape2::dcast(a, top + bottom ~ variable, value.var = 'value')
  
  # extract SSC
  .clay <- a.wide$claytotal_r
  .sand <- a.wide$sandtotal_r
  .silt <- a.wide$silttotal_r
  
  # truncate at 100%
  if(.sand + .silt + .clay > 100) {
    .silt <- 100 - (.sand + .clay)
  }
  
  ## convert Ksat units um/s --> m/d
  #
  # 1e-6 m / um
  # 60*60*24 = 86400 s / d
  #
  # um/s * 1e-6 m/um / (1/86400 s/d) --> m/d
  # um/s * 0.0864 ---------------------> m/d
  s$ksat_r <- s$ksat_r * 0.0864
  
  # Ksat of first mineral horizon
  # m/d
  .ksat0 <- s[, , .FIRST]$ksat_r
  
  ## Ksat decay parameter
  # dice(s, ~ ksat_r, SPC = FALSE)
  
  
  
  ## edit every possible component of the parameter file
  ## using our best interpretation of the SSURGO/RSS component data
  
  
  ## soil depth
  # convert cm -> m
  p$soil_depth <- .soildepth * 0.01
  
  ## soil depth used by heat flux model
  # set to soil depth
  # convert cm -> m
  p$deltaZ <- .soildepth * 0.01
  
  ## Saturated hydraulic conductivity at surface (meters / day)
  # using first mineral horizon
  p$Ksat_0 <- .ksat0
  
  ## Ksat decay function parameter
  p$m
  
  ## sand, silt, clay
  # convert percent -> fraction
  p$sand <- .sand * 0.01
  p$silt <- .silt * 0.01
  p$clay <- .clay * 0.01
  
  # done
  return(p)
}



# convert a soil parameter file to named list of values
soilParameterFileToList <- function(f) {
  
  # load as 2 column data.frame
  s <- read.table(f)
  
  # assign names, swap order
  names(s) <- c('value', 'parameter')
  s <- s[, c('parameter', 'value')]
  
  # convert to named vector -> list
  p <- s$value
  names(p) <- s$parameter
  p <- as.list(p)
  
  return(p)
}




# map soil parameters to SSURGO-derivatives
#
# https://github.com/RHESSys/RHESSys/wiki/Parameter-Definition-Files#soil-definition-file-parameters

toParameterNames <- function(i) {
  
  list(
    # ID is the map unit key
    patch_default_ID = i$mukey,
    
    # wt. mean over soil horizons, derived from SSURGO
    sand = i$sandtotal_r,
    silt = i$silttotal_r,
    clay = i$claytotal_r,
    
    # soil depth, to contact if present, otherwise bottom depth of component
    soil_depth = i$soil.depth,
    
    # via lm(log(x) ~ hz mid point)
    # model often unsuitable, or does not converge (n too small, constant values)
    # Ksat_0 = i$ksat_0,
    # porosity_0 = i$por_0,
    
    # estimated by wt. geometric mean over component to contact
    Ksat_0 = i$ksat_r,
    
    # estimated by wt. mean over component to contact
    porosity_0 = i$wsatiated_r,
    
    # constants
    m = 0.12,
    psi_max = 0.01
    
    ## TODO: what are these supposed to mean / how can we estimate?
    # interpretations of SSURGO data
    # psi_air_entry = 'XX',
    # pore_size_index = 'XX'
    
    # all other parameters are defaults
  )
  
}


# write named list to soil definition file

writeSoilDefinitionFile <- function(p, f = '') {
 
  # names for iteration
  nm <- names(p)
  
  # file length depends on the number of parameters
  textLines <- vector(mode = 'character', length = length(p)) 
  
  for(i in seq_along(p)) {
    # current key-value pair
    .v <- p[[i]]
    .n <- nm[i]
    
    # encode with single-space delimiter
    # [value] [label]
    textLines[i] <- sprintf("%s %s", .v, .n)
  }
  
  # write to file
  cat(textLines, sep = '\n', file = f)
}












