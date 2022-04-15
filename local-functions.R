##
##
##




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





#' @title Build a soil parameter file from SSURGO component data.
#'
#' @param x 
#' @param id 
#' @param template 
#'
#' @return
#' @export
#'
#' @examples
buildParameterFileSSURGO <- function(x, id, template = NULL) {
  
  
  
  
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




