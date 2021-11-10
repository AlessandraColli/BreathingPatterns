####################################################################
################ Complete breathing tracks analysis ################
####################################################################

library(fda)
library(fdakmapp)

source("../src/smooth_breath_gcv.R")
source("../src/localmin.R")
source("../src/table_breaths.R")
source("../src/rescale.R")
source("../src/outlier_detection.R")
source("../src/plots.R")

### load data ###

name=''

data=read.table(paste(name,".dat",sep=''),header = F)  

time=data[,1]
volrcp=data[,2]
volrca=data[,3]
volab=data[,4]
voltot=data[,5]

### plot data for visual inspection ###

x11()
plot(time, voltot, col='black', type = 'l', main='CW volume', xlab='time (s)', ylab='volume (L)')

x11()
plot(time, volrcp, col='black', type = 'l', main='RCp volume', xlab='time (s)', ylab='volume (L)')

x11()
plot(time, volrca, col='black', type = 'l', main='RCa volume', xlab='time (s)', ylab='volume (L)')

x11()
plot(time, volab, col='black', type = 'l', main='AB volume', xlab='time (s)', ylab='volume (L)')

### find local minima ###
# (these minima params are generally good, might need to be tuned to the specific case)
min=find_local_min(time, voltot, peak_span=201, grid_coef=10, step=10, plot=1,spiky_min = F)

x11(); plot(time, voltot, type='l', main='Local minima', xlab='time (s)', ylab='volume (L)')
points(time[min$minidx], voltot[min$minidx], col='red',pch=19, cex=1.5)

### smooth curves ###
volumes.tot=table_breaths(time,voltot,min$minidx)
volumes.rcp=table_breaths(time,volrcp, min$minidx)
volumes.rca=table_breaths(time,volrca, min$minidx)
volumes.ab=table_breaths(time,volab, min$minidx)

d=min$deltas;
times=volumes.tot$times
for(j in seq(1,length(d)))
  times[,j]=times[,j]-times[1,j];


smoothed=matrix(NA, nrow=dim(volumes.tot$breaths)[1],ncol=dim(volumes.tot$breaths)[2])
smoothed1=matrix(NA, nrow=dim(volumes.tot$breaths)[1],ncol=dim(volumes.tot$breaths)[2])

for(j in 1:dim(volumes.tot$breaths)[2]){
  vec=which(!is.na(volumes.tot$breaths[,j]))
  sm=smooth_breath(volumes.tot$times[vec,j], volumes.tot$breaths[vec,j],grid_coef=5,plot = 0)  #smooth breaths one by one
  smoothed[vec,j]=sm$smoothed_curve
  smoothed1[vec,j]=sm$der1
}  

smoothed_rcp=matrix(NA, nrow=dim(volumes.tot$breaths)[1],ncol=dim(volumes.tot$breaths)[2])
smoothed_rcp1=matrix(NA, nrow=dim(volumes.tot$breaths)[1],ncol=dim(volumes.tot$breaths)[2])

for(j in 1:dim(volumes.tot$breaths)[2]){
  vec=which(!is.na(volumes.rcp$breaths[,j]))
  sm=smooth_breath(volumes.tot$times[vec,j], volumes.rcp$breaths[vec,j],grid_coef=5,plot = 0)  #smooth breaths one by one
  smoothed_rcp[vec,j]=sm$smoothed_curve
  smoothed_rcp1[vec,j]=sm$der1
}

smoothed_rca=matrix(NA, nrow=dim(volumes.tot$breaths)[1],ncol=dim(volumes.tot$breaths)[2])
smoothed_rca1=matrix(NA, nrow=dim(volumes.tot$breaths)[1],ncol=dim(volumes.tot$breaths)[2])

for(j in 1:dim(volumes.tot$breaths)[2]){
  vec=which(!is.na(volumes.rca$breaths[,j]))
  sm=smooth_breath(volumes.tot$times[vec,j], volumes.rca$breaths[vec,j],grid_coef=5,plot = 0)  #smooth breaths one by one
  smoothed_rca[vec,j]=sm$smoothed_curve
  smoothed_rca1[vec,j]=sm$der1
}

smoothed_ab=matrix(NA, nrow=dim(volumes.tot$breaths)[1],ncol=dim(volumes.tot$breaths)[2])
smoothed_ab1=matrix(NA, nrow=dim(volumes.tot$breaths)[1],ncol=dim(volumes.tot$breaths)[2])

for(j in 1:dim(volumes.tot$breaths)[2]){
  vec=which(!is.na(volumes.ab$breaths[,j]))
  sm=smooth_breath(volumes.tot$times[vec,j], volumes.ab$breaths[vec,j],grid_coef=5,plot = 0)  #smooth breaths one by one
  smoothed_ab[vec,j]=sm$smoothed_curve
  smoothed_ab1[vec,j]=sm$der1
}

x11(); matplot(times, smoothed, type='l', main='Breaths - CW volume', xlab='time (s)', ylab='volume (L)')
x11(); matplot(times, smoothed_rcp, type='l', main='Breaths - RCp volume', xlab='time (s)', ylab='volume (L)')
x11(); matplot(times, smoothed_rca, type='l', main='Breaths - RCa volume', xlab='time (s)', ylab='volume (L)')
x11(); matplot(times, smoothed_ab, type='l', main='Breaths - AB volume', xlab='time (s)', ylab='volume (L)')
 
x11(); matplot(times, smoothed1, type='l', main='First Derivatives - total volume', xlab='time (s)', ylab='')
x11(); matplot(times, smoothed_rcp1, type='l', main='First Derivatives - RCp volume', xlab='time (s)', ylab='')
x11(); matplot(times, smoothed_rca1, type='l', main='First Derivatives - RCa volume', xlab='time (s)', ylab='')
x11(); matplot(times, smoothed_ab1, type='l', main='First Derivatives - AB volume', xlab='time (s)', ylab='')


### Outlier detection ###

filtered=outlier_detection(times,smoothed,smoothed_rcp,smoothed_rca,smoothed_ab,
                           plot_option=1)

plot_outliers(time, voltot, volumes.tot, filtered$time.outliers.idx, filtered$magnitude.outliers.idx, filtered$shape.outliers.idx,main='Outlier breaths - CW Volume')
plot_outliers(time, volrcp, volumes.rcp, filtered$time.outliers.idx, filtered$magnitude.outliers.idx, filtered$shape.outliers.idx,main='Outlier breaths - RCp Volume')
plot_outliers(time, volrca, volumes.rca, filtered$time.outliers.idx, filtered$magnitude.outliers.idx, filtered$shape.outliers.idx,main='Outlier breaths - RCa Volume')
plot_outliers(time, volab, volumes.ab, filtered$time.outliers.idx, filtered$magnitude.outliers.idx, filtered$shape.outliers.idx,main='Outlier breaths - AB Volume')


filtered.times=filtered$filtered.times
filtered.Vtot=filtered$filtered.Vtot
filtered.Vrcp=filtered$filtered.Vrcp
filtered.Vrca=filtered$filtered.Vrca
filtered.Vab=filtered$filtered.Vab
outliers=filtered$outliers.idx

filtered.Vtot1=smoothed1[,-outliers]
filtered.Vrcp1=smoothed_rcp1[,-outliers]
filtered.Vrca1=smoothed_rca1[,-outliers]
filtered.Vab1=smoothed_ab1[,-outliers]

filtered.Vtot1=filtered.Vtot1[1:dim(filtered.times)[1],]
filtered.Vrcp1=filtered.Vrcp1[1:dim(filtered.times)[1],]
filtered.Vrca1=filtered.Vrca1[1:dim(filtered.times)[1],]
filtered.Vab1=filtered.Vab1[1:dim(filtered.times)[1],]


### Clustering ###

# If absolute value is relevant-> groupBy.abs=TRUE, use L2. Use Pearson otherwise

groupBy.abs = T

if(groupBy.abs){
  
  datavec = c(c(t(filtered.Vtot)), c(t(filtered.Vrcp)), c(t(filtered.Vrca)),c(t(filtered.Vab)))
  arr=array(datavec, c(dim(filtered.Vtot)[2],dim(filtered.Vtot)[1], 4))
  
}else{
  
  datavec = c(c(t(filtered.Vtot1)), c(t(filtered.Vrcp1)), c(t(filtered.Vrca1)),c(t(filtered.Vab1)))
  arr=array(datavec, c(dim(filtered.Vtot1)[2],dim(filtered.Vtot1)[1], 4))

}

timet=t(filtered.times)

original.times=volumes.tot$times[,-outliers]   # for graphics
original.times=original.times[1:dim(filtered.times)[1],]

distance=ifelse(groupBy.abs==1,'l2','pearson')
warp= 'affine'           

find.median=F # FALSE to find clusters, TRUE to find just the median
K = 5 # max number of clusters to try. Selection of the final k will be done
# looking at the elbow in the plot of (k, similarity_within_cluster(k))

# NOTE
# the kmap function sets random start cluster centroids every time it's called
# hence it might produce different results if called multiple times

if(find.median){
  
  nclust=1
  
  res=kmap(timet, arr, n_clust=nclust, warping_method=warp, 
           center_method='medoid', similarity_method=distance, comp_original_center = T)
  
  kmap_show_results(res)
  
  # centroid label
  ind=which(res$similarity.final==0) 

  
  }else{
  
  # find the best number of clusters --> elbow of the (k, similarity) plot

  checksim=numeric(K)
    
  for(j in 1:K){
      
      check=kmap(timet, arr, n_clust=j, warping_method=warp, 
                 center_method='medoid', similarity_method=distance, comp_original_center = T)
      
      checksim[j]=mean(check$similarity.final)
  }
    
  x11()
  plot(checksim, type='l', xlab='Cluster labels', ylab='Mean similarities', main=paste0('Mean Similarity - ',warp,sep=''))
  points(checksim, pch=1, col='blue',lwd=2)
  
}
  
# select the best k among 1..K and perform clustering 
nclust=2

res=kmap(timet, arr, n_clust=nclust, warping_method=warp, 
         center_method='medoid', similarity_method=distance, comp_original_center = T)

kmap_show_results(res)

labels=res$labels
table(labels)

col=plot.breaths.byclusters(time,voltot,original.times,filtered.Vtot,labels)
plot.breaths.byclusters(time,volrcp,original.times,filtered.Vrcp,labels)
plot.breaths.byclusters(time,volrca,original.times,filtered.Vrca,labels)
plot.breaths.byclusters(time,volab,original.times,filtered.Vab,labels)

x11();matplot(filtered.times, filtered.Vtot,col=col$col.groups, type='l', main='Clustered breaths - CW', xlab= 'time (s)', ylab='volume (L)')
x11();matplot(filtered.times, filtered.Vrcp,col=col$col.groups, type='l',main='Clustered breaths - RCp', xlab='time (s)', ylab='volume (L)')
x11();matplot(filtered.times, filtered.Vrca,col=col$col.groups, type='l', main='Clustered breaths - RCa', xlab='time (s)', ylab='volume (L)')
x11();matplot(filtered.times, filtered.Vab,col=col$col.groups, type='l', main='Clustered breaths - AB', xlab='time (s)', ylab='volume (L)')

# find centroid labels  
if(groupBy.abs){
  ind=which(res$similarity.final==0)
}else{
  ind=which(res$similarity.final>=(-1-10^-15) & res$similarity.final<=(-1+10^-15))
}

labels[ind]
