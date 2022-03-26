#' Rescales a time, value vector of generic length into a vector of input length
#' 
#' @param x time vector
#' @param y value vector
#' @param n output length
#' 
#' @return a new y of length n
#' 
rescale_time=function(x,y,n=1000){      
  
  # step=length(x)/n
  f=x[1];
  t=x[length(x)];
  
  newx=seq(f,t,length.out=n);
  
  res_curve=spline(x,y,xout=newx);
  res_curve$y
}

#' Function that takes a time series as input and returns the value of the ts in n equidistanced
#' points over t_min and t_max, using a cubic spline for interpolation
#' 
#' @param breaths matrix of breath volumes
#' @param times matrix of breath time vectors
#' 
rescale_all=function(breaths,times){

  first_br=breaths
  first_time=times
  N=dim(breaths)[2];
  resc_br=matrix(0,100,N)
  grid=seq(0,100,length.out = 100)
  for(i in 1:N){
    # Remove NA 
    x=first_time[!is.na(first_time[,i]),i];
    y=first_br[!is.na(first_br[,i]),i];
    
    resc_br[,i] = rescale_time(x,y,n=100)
  }
  
  result=data.frame(resc_br)
  result
  
}
