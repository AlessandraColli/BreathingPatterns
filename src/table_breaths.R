### This function creates a table of separated breath curves ###
### and times given the signal and local minima indices      ###

table_breaths=function(time,amplitude, min_idx){
  
  breaths=amplitude[min_idx[1]:min_idx[2]];
  t=time[min_idx[1]:min_idx[2]];
  
  PLOT_BREATH = matrix(NA, nrow = length(time), ncol = length(min_idx)-1);
  PLOT_TIME = matrix(NA, nrow = length(time), ncol = length(min_idx)-1);
  #empty matrices for breaths plot, filled with NA
  PLOT_BREATH[1:length(breaths),1]=breaths;
  PLOT_TIME[1:length(breaths),1]=t;
  nrows=0;
  
  for ( i in seq(3, length(min_idx), by =1)  ){
    
    PLOT_BREATH[1:length(amplitude[min_idx[i-1]:min_idx[i]]),i-1] = amplitude[min_idx[i-1]:min_idx[i]];
    PLOT_TIME[1:length(time[min_idx[i-1]:min_idx[i]]),i-1] = time[min_idx[i-1]:min_idx[i]];
    if( length(amplitude[min_idx[i-1]:min_idx[i]]) > nrows) {
      nrows = length(amplitude[min_idx[i-1]:min_idx[i]])
    }   
  }
  PLOT_BREATH = PLOT_BREATH[1:nrows,];
  PLOT_TIME = PLOT_TIME[1:nrows,];
  
  result=list(breaths=PLOT_BREATH, times=PLOT_TIME);
  result
}