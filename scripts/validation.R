##############################################################
################ Procedure validation script  ################
##############################################################

library(fda)
library(fdakmapp)
library(roahd)
library(splus2R)

source("../R/smooth_breath_gcv.R")
source("../R/localmin.R")
source("../R/table_breaths.R")
source("../R/rescale.R")
source("../R/outlier_detection.R")
source("../R/plots.R")

setwd("../data")

# load artificial data
data=read.table('veryregular.txt',header=T)[,1]
time=(0:(length(data)-1))/60

# add noise
set.seed(8922961)
vol=data+rnorm(length(data), mean=0, sd=0.1)

# find local minima
# (these minima params are generally good, might need to be tuned to the specific case)
min=find_local_min(time, vol, peak_span=201, grid_coef=10, step=10, plot=1,spiky_min = F)

# smooth curves
volumes.tot=table_breaths(time,vol,min$minidx)

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

# outlier detection (univariate)
filtered=find_outliers(times,smoothed, plot_option=1, range=1.5, no_iter=0)

filtered.times=filtered$filtered.times
filtered.Vtot=filtered$filtered.Vtot

plot_outliers(time, vol, volumes.tot, filtered$time.outliers.idx, filtered$magnitude.outliers.idx, filtered$shape.outliers.idx,main='Outlier breaths - CW Volume', legend_pos='bottomleft')

datavec = c(c(t(filtered.Vtot)))
arr=array(datavec, c(dim(filtered.Vtot)[2],dim(filtered.Vtot)[1], 1))

timet=t(filtered.times)
outliers=filtered$outliers.idx
original.times=volumes.tot$times[,-outliers]   # for graphics
original.times=original.times[1:dim(filtered.times)[1],]


# find the best number of clusters (alternance.txt)

# the kmap function sets random start clusters centroids every time it's called
# hence it might produce different results if called multiple times
distance='l2'
warp='affine'

K=4
checksim=numeric(K)

for(j in 1:K){
  
  check=kmap(timet, arr, n_clust=j, warping_method=warp, 
             center_method='medoid', similarity_method=distance, comp_original_center = T)
  
  checksim[j]=mean(check$similarity.final)
}

x11()
plot(checksim, type='l', xlab='Cluster labels', ylab='Mean dissimilarities', main=paste0('Mean Dissimilarity - ',warp,sep=''))
points(checksim, pch=1, col='blue',lwd=2)

# perform clustering with the best number of clusters
nclust=2

res=kmap(timet, arr, n_clust=nclust, warping_method=warp, 
         center_method='medoid', similarity_method=distance, comp_original_center = T)

kmap_show_results(res)

labels=res$labels
table(labels)
col=plot.breaths.byclusters(time,vol,original.times,filtered.Vtot,labels)
