#' Compute sample kurtosis
#' 
#' @param x A numeric vector
#' 
#' @return The sample kurtosis of \code{x}.
#' 
#' @details \code{sample_skewness} will return an error if \code{x} contains NA values; address
#' missing data before using \code{sample_skewness}.
#' 
#' @examples
#' x1 <- rnorm(27)
#' sample_kurtosis(x1)
#' 
#' @author Jonathan Walter, \email{jawalter@@ucdavis.edu}
#' 
#' @export

sample_kurtosis<-function(x){
  
  if(any(is.na(x))){
    stop("x contains one or more NA values")
  }
  
  n = length(x)
  xbar = mean(x)
  m4 = (1/n)*sum((x-xbar)^4)
  m2 = ((1/n)*sum((x-xbar)^2))^2
  return(m4/m2-3)
  
}