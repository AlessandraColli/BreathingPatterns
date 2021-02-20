### This function performs local minima detection  ###
### following the algorithm described in the paper ###

library(splus2R)
source("smooth_breath_gcv.R")

find_local_min=function(time, voltot, peak_span=201, grid_coef=10, step=10, plot=0, spiky_min=F){
  
  # find local maxima
  localmax=which(peaks(voltot, span=peak_span)==TRUE)
  
  if(spiky_min==F){    # long breaths, with long "tails"
  br=table_breaths(time,voltot,localmax)
  
  min=NULL
  minidx=NULL
  
  for(j in 1:dim(br$breaths)[2]){       #smoothing to find piecewise derivatives
    vec=which(!is.na(br$breaths[,j]))
    sm=smooth_breath(br$times[vec,j], br$breaths[vec,j],grid_coef=grid_coef,plot = 0)  #smooth breaths one by one
    
    timem=br$times[vec,j]
    volm=br$breaths[vec,j]
    der1m=sm$der1
    
    if(length(timem)>step){
      
      # divide the portion between two consecutive maxima  
      # and compute the mean derivative in each portion
      
      amp=length(timem)%%step
      
      breaks=seq(1,(length(timem)-amp),by=step)
      
      minloc=NULL
      
      if(length(breaks)>2){                     
        
        ms=c(mean(der1m[breaks[1]:breaks[2]]))
        
        for(j in 2:(length(breaks)-1)){
          med=mean(der1m[breaks[j]:breaks[j+1]])
          ms=c(ms,med)
          # look in which portion the mean derivative starts to grow significantly
          if(med>=0 & med>3*ms[j-1]){
            id=which.min(volm[breaks[j-1]:breaks[j+1]])
            minloc=timem[breaks[j-1]+id-1]
          }
        }
      }else if(length(breaks)==2){
        id=which.min(volm[breaks[1]:breaks[2]])
        minloc=timem[breaks[1]+id-1]
      }
      
      min=c(min,minloc)
      minidx=c(minidx,which(time==minloc))
    }
    
  }
  
  }else{ # high-frequency breathing, breaths with no tails -> find local minima directly
    
    minidx=which(peaks(-voltot, span=peak_span)==TRUE)
    min=voltot[minidx]
  }
  
  # compute breaths lengths
  deltaT=c();
  last=time[minidx[1]];
  
  for (i in 2:(length(min))){
    t=time[minidx[i]];
    deltaT=c(deltaT,abs(t-last));
    last=t;
  }
  
  if(plot){
    
    x11(); plot(time,voltot,type='l',main='Local maxima'); points(time[localmax],voltot[localmax],col='red',pch=20)
    x11()
    plot(time,voltot,type='l', main='Local minima')
    points(time[minidx],voltot[minidx],pch=20,col='red')
  }
  
  result=list(minima=min, minidx=minidx, maxidx=localmax, deltas=deltaT,
              peak_span = peak_span, grid_coef = grid_coef, step = step,
              spiky_min = spiky_min)
  
  result
  
}
