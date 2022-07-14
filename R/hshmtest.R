#' Test for presence of hot spots/hot moments (hshm) in a set of observations
#' 
#' @param x A numeric vector
#' @param stat The distributional moment to evaluate; one of 'skewness'
#' or 'kurtosis'.
#' @param nreps The number of replicates in the parametric bootstrapping procedure
#' to use for assessing statistical significance. Defaults to 1000.
#' 
#' @return A list. Slots are:
#' \item{emp}{The empirical skewness or kurtosis (depending on \code{stat})}
#' \item{surr}{The skewness or kurtosis of surrogate datasets generated using parametric
#' bootstrapping}
#' \item{quantile}{The quantile of the empirical skewness or kurtosis relative to that 
#' of surrogates. Used to determine statistical significance: see Details.}
#' 
#' @details The parametric bootstrapping procedure used to determine statistical 
#' significance compares the skewness or kurtosis of the distribution of observations
#' in \code{x} to that of surrogate datasets created by drawing \code{length(x)}
#' random deviates from a reference distribution with the same mean and variance
#' as \code{x}. Currently only the normal distribution is implemented; the normal 
#' distribution has skewness = 0 and excess kurtosis = 0. The quantile of the empirical
#' HSHM statistic relative to that of surrogates can be interpreted as an indicator
#' of statistical significance; for example, a value >0.95 corresponds to an
#' HSHM statistic significantly greater than expected by chance using a 1-tailed test
#' at a type-1 error rate of 0.05.
#' 
#' @examples 
#' 
#' set.seed(11)
#' 
#' #no hshm
#' x1 <- rnorm(50)
#' t1 <- hshmtest(x1, stat="skewness")
#' print(t1$emp)
#' print(t1$quantile)
#' 
#' #with hshm
#' x2 <- x1
#' x2[rank(x2) >=45] <- x2[rank(x2) >= 45]*1.6
#' t2 <- hshmtest(x2, stat="skewness")
#' print(t2$emp)
#' print(t2$quantile)
#' 
#' @export


hshmtest<-function(x, stat, nreps=1000){
  # compare empirical skewness to measured skewness of a symmetric distribution with same mean and variance
  
  if(any(is.na(x))){
    stop("sample contains one or more NA values")
  }
  
  if(!stat %in% c("skewness","kurtosis")){
    stop("stat must be skewness or kurtosis")
  }
  
  n = length(x)
  mu.x = mean(x)
  sd.x = sd(x)
  
  if(stat=="skewness"){
    emp.skew = sample_skewness(x)
    surr.skew<-rep(NA, nreps)
    for(ii in 1:nreps){
      surr.x<-rnorm(n, mu.x, sd.x)
      surr.skew[ii]<-sample_skewness(surr.x)
    }
    
    qq<-rank(c(emp.skew,surr.skew))[1]/(nreps+1)
    
    return(list(emp=emp.skew, surr=surr.skew, quantile=qq))
  }
  
  if(stat=="kurtosis"){
    emp.kurt = sample_kurtosis(x)
    surr.kurt<-rep(NA, nreps)
    for(ii in 1:nreps){
      surr.x<-rnorm(n, mu.x, sd.x)
      surr.kurt[ii]<-sample_kurtosis(surr.x)
    }
    
    qq<-rank(c(emp.kurt,surr.kurt))[1]/(nreps+1)
    
    return(list(emp=emp.kurt, surr=surr.kurt, quantile=qq))
  }
  
}
