##
##
##

## TODO: 
#  * constant values?
#  * not enough values to fit a curve? dice() -> nls() | optim() -> coef()
#  * are exponential fits reasonable?

fitDecayFunction <- function(z, p0, p) {
  
  # solve for p
  # res <- p0 * exp(-(z/p))
  
  return(res)
  
}



dominantCondition <- function(i, v) {
  
  i <- i[which(i$compkind != 'Miscellaneous area'), ]
  if(nrow(i) < 1) {
    return(NULL)
  }
  
  fm <- as.formula(sprintf("comppct_r ~ %s", v))
  a <- aggregate(fm, data = i, FUN = sum, na.rm = TRUE)
  
  idx <- order(a[['comppct_r']], decreasing = TRUE)[1]
  
  res <- data.frame(
    mukey = i$mukey[1],
    v = a[[v]][idx],
    pct = a[['comppct_r']][idx]
  )
  
  names(res) <- c('mukey', v, 'pct')
  
  return(res)
}

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




writeSoilParameterFile <- function(p, f = '') {
 
  # names for iteration
  nm <- names(p)
  
  # file length depends on the number of parameters
  textLines <- vector(mode = 'character', length = length(p)) 
  
  for(i in seq_along(p)) {
    # current key-value pair
    .v <- p[[i]]
    .n <- nm[i]
    
    # encode with single-space delimeter
    # [value] [label]
    textLines[i] <- sprintf("%s %s", .v, .n)
  }
  
  # write to file
  cat(textLines, sep = '\n', file = )
}




