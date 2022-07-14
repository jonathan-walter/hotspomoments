#' Compute sample skewness
#' 
#' @param x A numeric vector
#' 
#' @return The sample skewness of \code{x}.
#' 
#' @details This function uses the definition of sample skewness by Joanes & Gill (1998),
#' which was shown to perform well in small samples from non-normal distributions. 
#' \code{sample_skewness} will return an error if \code{x} contains NA values; address
#' missing data before using \code{sample_skewness}.
#' 
#' @references Joanes, D. N.; Gill, C. A. (1998). "Comparing measures of sample skewness and kurtosis". 
#' Journal of the Royal Statistical Society, Series D. 47 (1): 183–189. doi:10.1111/1467-9884.00122.
#' 
#' @examples
#' x1 <- rnorm(27)
#' sample_skewness(x1)
#' 
#' @author Jonathan Walter, \email{jawalter@@ucdavis.edu}
#' 
#' @export


sample_skewness<-function(x){
  #from Joanes, D. N.; Gill, C. A. (1998). "Comparing measures of sample skewness and kurtosis". 
  # Journal of the Royal Statistical Society, Series D. 47 (1): 183–189. doi:10.1111/1467-9884.00122.
  #this form is shown to have low RMSE in small samples from non-normal distributions
  if(any(is.na(x))){
    stop("x contains one or more NA values")
  }
  
  n = length(x)
  xbar = mean(x)
  m3 = (1/n)*sum((x-xbar)^3)
  s3 = ((1/(n-1))*sum((x-xbar)^2))^(3/2)
  c = sqrt(n*(n-1))/(n-2)
  return(c*m3/s3)
  
}