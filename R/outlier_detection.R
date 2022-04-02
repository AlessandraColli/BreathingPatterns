#' This function performs functional outlier detection on a set of Optoelectronic Plethysmography
#' curves containing total volume, pulmonary rib cage voltume, abdominal rib cage volume and abdominal volume
#' as in the paper *A. LoMauro, A. Colli, L. Colombo, A. Aliverti,
#' Breathing patterns recognition: A functional data analysis approach,
#' Computer Methods and Programs in Biomedicine*
#'
#' @param times matrix of time vectors for each breath curve
#' @param smoothed_tot matrix of (smoothed) total volumes
#' @param smoothed_tot matrix of (smoothed) pulmonary rib cage volumes
#' @param smoothed_rcp matrix of (smoothed) abdominal rib cage volumes
#' @param smoothed_rcp matrix of (smoothed) abdominal volumes
#' @param plot_option defaults 0, if 1 plots all outlier detection results
#' @param weights a vector of weighting of each compartment to be used in multivariate
#' depth measures. if "uniform", 0.25 weights are used for each compartment
#' @param range time boxplot range (defaults to 1.5, that is the classical 1.5 IQR to mark outliers)
#' @param no_iter defaults 0, if 1 outlier detection iterations are not performed. To be used e.g. when breaths are few
#' 
#' @return a list with a matrix of filtered time curves and compartment curves,
#' as well as outliers indexes

### multivariate signal (Vtot, RCp, RCa, AB) ###
outlier_detection=function(times,smoothed_tot,smoothed_rcp,smoothed_rca,smoothed_ab,plot_option=1, weights='uniform', range=1.5, no_iter=0){
  
  filtered.times=times
  filtered.Vtot=smoothed_tot
  filtered.Vrcp=smoothed_rcp
  filtered.Vrca=smoothed_rca
  filtered.Vab=smoothed_ab
  
  # Check if the number of breaths is high enough to safely apply the entire procedure
  breaths.are.too.few=max(ifelse(dim(filtered.Vtot)[2]<30, 1, 0), no_iter)
  
  if(breaths.are.too.few)
    warning('Number of breaths is too low to apply outlier detection iteratively. Outlier checks have been performed only once')
  
  # Auxiliary vector to store the original indices of the outliers
  aux.idx=1:dim(smoothed_tot)[2]  
  
  ##### outlier detection: TIME
  time.outliers.idx=NULL # vector of indices for time outliers
  found=1
  
  deltas=c()
  for(i in 1:dim(filtered.times)[2])
    deltas=c(deltas, filtered.times[length(which(!is.na(filtered.times[,i]))),i])
  
  while(found){             # iterate detection until no outliers are found
    if( plot_option == T){x11()}
    bp <- boxplot(deltas, range=range, plot = plot_option)
    timesout <- bp$out
    timesout <- unique(timesout)
    
    out <- NULL
    for(k in 1:length(deltas)){
      for(h in timesout)
        if(deltas[k]==h){
          out <- c(out,k)
          break
        }
    }
    
    # remove outliers in time (if any)
    if(length(out)>0){
      deltas=deltas[-out]
      
      filtered.times=filtered.times[,-out]
      filtered.Vtot=filtered.Vtot[,-out]
      filtered.Vrcp=filtered.Vrcp[,-out]
      filtered.Vrca=filtered.Vrca[,-out]
      filtered.Vab=filtered.Vab[,-out]
      
      time.outliers.idx=c(time.outliers.idx, aux.idx[out])
      aux.idx=aux.idx[-out]
      
      if(breaths.are.too.few)   # Stop to 1 iteration if there are not many breaths
        found=0
      
      
    }
    else
      found=0
  }
  
  # Rescale breaths on [1:100] to apply functional outlier detection on magnitude and shape
  scaled.Vtot=rescale_all(filtered.Vtot,filtered.times)
  scaled.Vrcp=rescale_all(filtered.Vrcp,filtered.times)
  scaled.Vrca=rescale_all(filtered.Vrca,filtered.times)
  scaled.Vab=rescale_all(filtered.Vab,filtered.times)
  
  ##### outlier detection: MAGNITUDE
  magnitude.outliers.idx=NULL  # vector of indices for magnitude outliers
  found=1
  
  while(found){             # iterate detection until no outliers are found
    
    #  x11()
    out1=fbplot(mfData(grid=1:100,list(t(scaled.Vtot),t(scaled.Vrcp), t(scaled.Vrca),t(scaled.Vab))), Depths=list(def='MBD',weights=weights), main=list('Magnitude outliers','Magnitude outliers','Magnitude outliers','Magnitude outliers'),display=plot_option)
    idx1=out1$ID_outliers;
    
    # remove outliers in magnitude (if any)
    if(length(as.vector(idx1))>0){
      scaled.Vtot=scaled.Vtot[,-idx1]
      scaled.Vrcp=scaled.Vrcp[,-idx1]
      scaled.Vrca=scaled.Vrca[,-idx1]
      scaled.Vab=scaled.Vab[,-idx1]
      
      filtered.times=filtered.times[,-idx1]
      filtered.Vtot=filtered.Vtot[,-idx1]
      filtered.Vrcp=filtered.Vrcp[,-idx1]
      filtered.Vrca=filtered.Vrca[,-idx1]
      filtered.Vab=filtered.Vab[,-idx1]
      
      magnitude.outliers.idx=c(magnitude.outliers.idx, aux.idx[idx1])
      aux.idx=aux.idx[-idx1]
      
      if(breaths.are.too.few)     # Stop to 1 iteration if there are not many breaths
        found=0
    }
    else 
      found=0
  }
  
  ##### outlier detection: SHAPE
  shape.outliers.idx=NULL # vector of indices for shape outliers
  found=1
  
  while(found){   # iterate detection until no outliers are found
    if(plot_option==TRUE){x11()}
    out2=multivariate_outliergram(mfData(grid=1:100,list(t(scaled.Vtot),t(scaled.Vrcp), t(scaled.Vrca),t(scaled.Vab))),weights=weights, display = plot_option) #outlier detection-shape
    idx2=out2$ID_outliers;
    if(length(as.vector(idx2))>0){
      
      scaled.Vtot=scaled.Vtot[,-idx2]
      scaled.Vrcp=scaled.Vrcp[,-idx2]
      scaled.Vrca=scaled.Vrca[,-idx2]
      scaled.Vab=scaled.Vab[,-idx2]
      
      filtered.times=filtered.times[,-idx2]
      filtered.Vtot=filtered.Vtot[,-idx2]
      filtered.Vrcp=filtered.Vrcp[,-idx2]
      filtered.Vrca=filtered.Vrca[,-idx2]
      filtered.Vab=filtered.Vab[,-idx2]
      
      shape.outliers.idx=c(shape.outliers.idx, aux.idx[idx2])
      aux.idx=aux.idx[-idx2]
      
      if(breaths.are.too.few)    # Stop to 1 iteration if there are not many breaths
        found=0
    }
    else
      found=0
  }
  
  # Cut breaths to the longest one
  max.len=0
  for(j in 1:dim(filtered.times)[2]){
    len=length(which(!is.na(filtered.times[,j])))
    if(len>max.len)
      max.len=len
  }
  
  filtered.times=filtered.times[1:max.len,]
  filtered.Vtot=filtered.Vtot[1:max.len,]
  filtered.Vrcp=filtered.Vrcp[1:max.len,]
  filtered.Vrca=filtered.Vrca[1:max.len,]
  filtered.Vab=filtered.Vab[1:max.len,]
  
  
  result=list(
    filtered.times=filtered.times,
    filtered.Vtot=filtered.Vtot,
    filtered.Vrcp=filtered.Vrcp,
    filtered.Vrca=filtered.Vrca,
    filtered.Vab=filtered.Vab,
    outliers.idx=c(time.outliers.idx, magnitude.outliers.idx,shape.outliers.idx),
    time.outliers.idx=time.outliers.idx,
    magnitude.outliers.idx=magnitude.outliers.idx,
    shape.outliers.idx=shape.outliers.idx,
    weights=weights, range=range, no_iter=no_iter)
  
}

#' This function performs functional outlier detection on a set of breathing
#' curves containing only the total volume
#' as in the paper *A. LoMauro, A. Colli, L. Colombo, A. Aliverti,
#' Breathing patterns recognition: A functional data analysis approach,
#' Computer Methods and Programs in Biomedicine*
#'
#' @param times matrix of time vectors for each breath curve
#' @param smoothed_tot matrix of (smoothed) total volumes
#' @param plot_option defaults 0, if 1 plots all outlier detection results
#' @param weights a vector of weighting of each compartment to be used in multivariate
#' depth measures. if "uniform", 0.25 weights are used for each compartment
#' @param range time boxplot range (defaults to 1.5, that is the classical 1.5 IQR to mark outliers)
#' @param no_iter defaults 0, if 1 outlier detection iterations are not performed. To be used e.g. when breaths are few
#' 
#' @return a list with a matrix of filtered time curves and volume curves,
#' as well as outliers indexes

### univariate signal (Vtot only) ###
find_outliers=function(times,smoothed_tot,plot_option=1, weights='uniform', range=1.5, no_iter=0){
  
  filtered.times=times
  filtered.Vtot=smoothed_tot
  
  # Check if the number of brethas is high enough to safely apply the entire procedure
  breaths.are.too.few=max(ifelse(dim(filtered.Vtot)[2]<30, 1, 0), no_iter)
  
  if(breaths.are.too.few)
    warning('Number of breaths is too low to apply outlier detection iteratively. Outlier checks have been performed only once')
  
  # Auxiliary vector to store the original indices of the outliers
  aux.idx=1:dim(smoothed_tot)[2]  
  
  ##### outlier detection: TIME
  time.outliers.idx=NULL # vector of indices for time outliers
  found=1
  
  #deltas=min$deltas
  
  deltas=c()
  for(i in 1:dim(filtered.times)[2])
    deltas=c(deltas, filtered.times[length(which(!is.na(filtered.times[,i]))),i])
  
  while(found){             # iterate detection until no outliers are found
    if( plot_option == T){x11()}
    bp <- boxplot(deltas, range=range, plot = plot_option)
    timesout <- bp$out
    timesout <- unique(timesout)
    
    out <- NULL
    for(k in 1:length(deltas)){
      for(h in timesout)
        if(deltas[k]==h){
          out <- c(out,k)
          break
        }
    }
    
    # remove outliers in time (if any)
    if(length(out)>0){
      deltas=deltas[-out]
      
      filtered.times=filtered.times[,-out]
      filtered.Vtot=filtered.Vtot[,-out]
      
      time.outliers.idx=c(time.outliers.idx, aux.idx[out])
      aux.idx=aux.idx[-out]
      
      if(breaths.are.too.few)   # Stop to 1 iteration if there are not many breaths
        found=0
      
      
    }
    else
      found=0
  }
  
  # Rescale breaths on [1:100] to apply functional outlier detection on magnitude and shape
  scaled.Vtot=rescale_all(filtered.Vtot,filtered.times)
  
  ##### outlier detection: MAGNITUDE
  magnitude.outliers.idx=NULL  # vector of indices for magnitude outliers
  found=1
  
  while(found){             # iterate detection until no outliers are found
    
    if( plot_option == T){x11()}
    out1=fbplot(fData(grid=1:100, t(scaled.Vtot)), main=list('Magnitude outliers'),display=plot_option)
    idx1=out1$ID_outliers;
    
    # remove outliers in magnitude (if any)
    if(length(as.vector(idx1))>0){
      scaled.Vtot=scaled.Vtot[,-idx1]
      
      filtered.times=filtered.times[,-idx1]
      filtered.Vtot=filtered.Vtot[,-idx1]
      
      magnitude.outliers.idx=c(magnitude.outliers.idx, aux.idx[idx1])
      aux.idx=aux.idx[-idx1]
      
      if(breaths.are.too.few)     # Stop to 1 iteration if there are not many breaths
        found=0
    }
    else 
      found=0
  }
  
  ##### outlier detection: SHAPE
  shape.outliers.idx=NULL # vector of indices for shape outliers
  found=1
  
  while(found){   # iterate detection until no outliers are found
    if( plot_option == T){x11()}
    out2=outliergram(fData(grid=1:100,t(scaled.Vtot)), display = plot_option) #outlier detection-shape
    idx2=out2$ID_outliers;
    if(length(as.vector(idx2))>0){
      
      scaled.Vtot=scaled.Vtot[,-idx2]
      
      filtered.times=filtered.times[,-idx2]
      filtered.Vtot=filtered.Vtot[,-idx2]
      
      shape.outliers.idx=c(shape.outliers.idx, aux.idx[idx2])
      aux.idx=aux.idx[-idx2]
      
      if(breaths.are.too.few)    # Stop to 1 iteration if there are not many breaths
        found=0
    }
    else
      found=0
  }
  
  # Cut breaths to the longest one
  max.len=0
  for(j in 1:dim(filtered.times)[2]){
    len=length(which(!is.na(filtered.times[,j])))
    if(len>max.len)
      max.len=len
  }
  
  filtered.times=filtered.times[1:max.len,]
  filtered.Vtot=filtered.Vtot[1:max.len,]
  
  
  result=list(
    filtered.times=filtered.times,
    filtered.Vtot=filtered.Vtot,
    outliers.idx=c(time.outliers.idx, magnitude.outliers.idx,shape.outliers.idx),
    time.outliers.idx=time.outliers.idx,
    magnitude.outliers.idx=magnitude.outliers.idx,
    shape.outliers.idx=shape.outliers.idx,
    weights=weights, range=range, no_iter=no_iter)
  
}
