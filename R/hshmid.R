#' Identify hot spot/hot moment (HSHM) observations
#' 
#' @param x A numeric vector
#' @param criteria The criteria for identifying HSHM. Must be one of "reduce.skew", 
#' "reduce.kurtosis" or "ref.normal".
#' @param thresh The threshold value used to delineate HSHM observations; see Details.
#' @param side The side of the distribution to identify HSHM on; one of "upper", "lower"
#' or "both". Ignored if 
#' 
#' @return A logical vector indicating whether the corresponding element in \code{x}
#' is a HSHM.
#' 
#' @details Currently, three criteria for identifying HSHM are implemented.
#' "reduce.skew" iteratively removes the most extreme observations until skewness
#' is \code{<= thresh}. The lowest observations are removed if \code{x} is
#' left-skewed; the largest observations if \code{x} is right-skewed. The removed 
#' observations constitute HSHM. "reduce.kurtosis" removes the most extreme observations
#' from both sides of the distribution until excess kurtosis is \code{<= thresh}.
#' "ref.normal" determines the values quantile(s) of a normal distribution corresponding to 
#' \code{thresh} and identifies as HSHM observations in \code{x} that are more extreme 
#' than the critical value(s). If \code{side} is "upper" or "lower" a one-tailed approach is used;
#' if \code{side} is "both" a two-tailed approach is used.
#' 
#' @examples 
#' set.seed(11)
#' x1 <- rnorm(50)
#' x1[rank(x2) >=45] <- x1[rank(x2) >= 45]*1.6 #create HSHM obs
#' hshmobs <- hshmid(x1, criteria="ref.normal", side="upper", thresh=0.95)

hshmid<-function(x, criteria, thresh, side=NULL){
  #Identify which observations constitute HSHM based on one of several criteria
  #returns a logical vector indicating whether observations in x constitute HSHM
  
  if(any(is.na(x))){
    stop("x contains one or more NA values")
  }
  
  if(!criteria %in% c("reduce.skew","reduce.kurtosis","ref.normal")){
    stop("critera must be one of 'reduce.skew', 'reduce.kurtosis' or 'ref.normal'")  
  }
  
  if(criteria=="reduce.skew"){ #reduce skewness to a chosen value
    
    tmpx <- x
    
    if(sign(sample_skewness(x))==-1){ #for negatively skewed data
      
      if(thresh <= sample_skewness(x)){
        stop("thresh <= negative sample skewness; no reduction possible")
      }
      
      for(ii in 1:length(x)){
        
        tmpx <- tmpx[-which.min(tmpx)] 
        if(sample_skewness(tmpx) >= thresh){
          break
        }
      }
      return(rank(x) <= ii)
    }
    
    else{ #for positively skewed data
      
      if(thresh >= sample_skewness(x)){
        stop("thresh >= positive sample skewness; no reduction possible")
      }
      
      for(ii in 1:length(x)){
        
        tmpx <- tmpx[-which.max(tmpx)] 
        if(sample_skewness(tmpx) <= thresh){
          break
        }
      }
      return(rank(-x) <= ii)
    }
  }
  
  
  if(criteria=="reduce.kurtosis"){ #reduce kurtosis to a chosen value
    
    if(thresh >= sample_kurtosis(x)){
      stop("thresh >= sample kurtosis; no reduction possible")
    }
    
    tmpx <- x
    hshm <- NULL
    
    if(Abs(min(x)) >= Abs(max(x))){ #if true, start with min(x)
      for(ii in 1:length(x)){
        if((ii %% 2) ==  0){
          hshm <- c(hshm, max(tmpx))
          tmpx <- tmpx[-which.max(tmpx)]
        }
        else{
          hshm <- c(hshm, min(tmpx))
          tmpx <- tmpx[-which.min(tmpx)]
        }
      }
    }
    else{
      for(ii in 1:length(x)){
        if((ii %% 2) ==  0){
          hshm <- c(hshm, min(tmpx))
          tmpx <- tmpx[-which.min(tmpx)]
        }
        else{
          hshm <- c(hshm, max(tmpx))
          tmpx <- tmpx[-which.max(tmpx)]
        }
      }
    }
    return(x %in% hshm)
  }
  
  
  if(criteria=="ref.normal"){ #one potential improvement would be to make this function two-sided
    
    if(is.null(side)){
      stop("a side of the distribution must be specified when criteria='ref.normal'")
    }
    
    if(!side %in% c("upper","lower","both")){
      stop("accepted values for 'side' are 'upper', 'lower' or 'both'")
    }
    
    if(thresh <= 0 | thresh >= 1){
      stop("value provided to thresh is incompatible with ref.normal criteria")
    }
    
    if(side=="upper"){
      crit <- qnorm(thresh, mean(x), sd(x))
      return(x > crit)
    }
    if(side=="lower"){
      crit <- qnorm(thresh, mean(x), sd(x))
      return(x < crit)
    }
    if(side=="both"){
      critlo <- qnorm((1-thresh)/2, mean(x), sd(x))
      crithi <- qnorm(1-(1-thresh)/2, mean(x), sd(x))
      return(x < critlo | x > crithi)
    }
  }
}