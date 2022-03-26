#' This function plots outlier curves over the total volume curve, in different colors 
#' according to the kind of outlier (time, magnitude, shape)
#' 
#' @param time time vector
#' @param vol volume vector
#' @param volumes matrix of single breaths volumes
#' @param time.outliers.idx indexes of time outliers wrt the breaths matrix
#' @param magnitude.outliers.idx indexes of magnitude outliers wrt the breaths matrix
#' @param shape.outliers.idx indexes of shape outliers wrt the breaths matrix
#' 
plot_outliers=function(time, vol, volumes,
                       time.outliers.idx, magnitude.outliers.idx, shape.outliers.idx,
                       main='Outlier breaths',xlab='time (s)', ylab='volume (L)', legend_pos='topright')
{
  col=c('red','blue','green')
  x11()
  plot(time, vol, type='l', xlab=xlab, ylab=ylab, main=main, col='black')
  
  for(j in time.outliers.idx){
    lines(volumes$times[,j],volumes$breaths[,j], type='l', col=col[1])
  }
  for(j in magnitude.outliers.idx){
    lines(volumes$times[,j],volumes$breaths[,j], type='l', col=col[2])
  }
  for(j in shape.outliers.idx){
    lines(volumes$times[,j],volumes$breaths[,j], type='l', col=col[3])
  }
  
  text=c('Outliers - Time','Outliers - Magnitude','Outliers - Shape')
  lty <- rep(1, length(text))
  lwd <- rep(2,length(text))
  legend(legend_pos, legend = text, col = col, lty = lty, lwd=lwd, cex = 0.8, bty = 'n')
  
}  

#' This function plots breath curves against the breathing track, in different color according
#' to the cluster they belong.
#' 
#' @param time time vector
#' @param amplitude volume vector
#' @param plot.times matrix of time vectors of the breaths to plot
#' @param plot.breaths matrix of volume vectors of the breaths to plot
#' @param labels labels of each breath
#' @param threshold clusters having a number of breaths smaller than threshold will be plotted as
#' belonging to a "Discarded" group.
#' 
#' @return a list with colors assigned to each breath and the corresponding cluster
#' 
plot.breaths.byclusters=function(time,amplitude,plot.times,plot.breaths, labels, legend_pos='topleft',threshold=0){
  
  n.breaths=dim(plot.times)[2]
  labels.unique=sort(unique(labels))
  n.clusters=length(labels.unique)
  
  
  col.ramp <- c("red", "blue", "green3", "orange", "grey", "yellow")
  col.ramp.after <- rainbow(n.clusters)
  
  col.ramp <- c(col.ramp,col.ramp.after)
  col.ramp <- col.ramp[1:n.clusters]
  cluster.dimension <- rep(0, n.clusters)
  
  col.bygroup <- rep(0, n.breaths)
  for(k in labels.unique){
    col.bygroup[which(labels == k)] <- col.ramp[k]
    cluster.dimension[k] <- sum(labels==k)                # DA VEDERE!
  }  
  cluster.prop <- (cluster.dimension/n.breaths)*100
  
  
  ### Plot each breath in the colour of the corresponding cluster
  
  x11()
  plot(time, amplitude, type='l', xlab='Time (s)', ylab='Volume (L)', main='Breaths by clusters', col='black')
  
  for(j in 1:n.breaths){
    lines(plot.times[,j],plot.breaths[,j], type='l', col=col.bygroup[j])
  }
  
  text <- rep(0, n.clusters)
  for (i in 1:n.clusters) {
    tt <- c("Cluster ", i, "  Number of breaths = ", cluster.dimension[i], "  (",round(cluster.prop[i],digits = 2), "%)")
    tt <- paste(tt, collapse = "")
    text[i] <- tt
  }
  lty <- rep(1, length(text))
  lwd <- rep(2,length(text));
  legend(legend_pos, legend = text, col = col.ramp, lty = lty, lwd=lwd, cex = 0.8, bty='n')
  
  ### Plot selected clusters only
  
  selected=rep(1,n.clusters)  # vettore di bool che indicano se il cluster è scartato (0) o tenuto (1)
  
  # Rappresento in grigio i respiri appartenenti ai cluster scartati
  col.bySelectedGroup <- col.bygroup
  for(i in 1:n.clusters){
    if(cluster.prop[i] <= threshold){
      selected[i]=0
      col.bySelectedGroup[which(labels == i)] <- "grey"
    }
  }
  
  num.discarded=length(which(selected==0))
  
  if(threshold!=0){  # if no threshold is set, show only the first plot
    
    x11()
    plot(time, amplitude, type='l', xlab='Time', ylab='Volume', main=paste(c('Selected clusters - Threshold = ',threshold, "%"), collapse=""), col='black')
    
    for(j in 1:n.breaths){
      lines(plot.times[,j],plot.breaths[,j], type='l', col=col.bySelectedGroup[j])
    }
    
    text <- NULL
    color.leg <-NULL
    for (i in 1:n.clusters) {
      if(selected[i]){ 
        tt <- c("Cluster ", i, "  Number of breaths = ", cluster.dimension[i], "  (",round(cluster.prop[i],digits = 2), "%)")
        tt <- paste(tt, collapse = "")
        text <- c(text, tt)
        color.leg <- c(color.leg, col.ramp[i])
      }
    }
    text <- c(text, paste("Discarded: ", ifelse(num.discarded>0, paste("Cluster ", which(selected==0)), "None"), collapse = ""))
    color.leg <- c(color.leg, "grey")
    lty <- rep(1, length(text))
    lwd <- rep(2,length(text));
    legend(legend_pos, legend = text, col = color.leg, lty = lty, lwd=lwd, cex = 0.8)
    
  }
  
  result=list(col.groups=col.bygroup, col.selected.groups=col.bySelectedGroup)
  
}

