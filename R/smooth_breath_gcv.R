#' Function for piecewise smoothing of breath curves using B-splines. The optimal smoothing
#' parameter lambda is chosen by minimising Generalised Cross Validation (GCV) error.
#' 
#' @param time time abscissa vector
#' @param amplitude the volume vector
#' @param order order of B-Splines to use for smoothing. Defaults to 5.
#' @param grid_coef length of pieces in piecewise smoothing. Defaults to 10.
#' @param lambda vector of smoothing parameters to try. The default goes from 1e-4 to 1e-10. 
#' This vector should have a logarithmic scale.
#' @param plot defaults 0, if 1 GCV minimisation results are plotted
#' 
#' @return a list with smoothed curves, the estimated first and second derivative, the
#' estimated GCV and degrees of freedom.
#' 
#' @references J.Ramsay , B. Silverman , Functional Data Analysis, Springer Series in Statistics, 2005 .
#' 
smooth_breath= function(time, amplitude, order=5, grid_coef=10,lambda=c(1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10),plot=0){
  
  breakst=c()
  for(i in seq(1,length(time),grid_coef))
    breakst=c(breakst,time[i])
  
  base=create.bspline.basis(c(time[0],time[length(time)]),breaks=breakst,norder=order)

  abscissa=time;
  Xobs0=amplitude;

  # Recommendation: look for the optimal smoothing parameter lambda on a log scale
  
  gcv=numeric(length(lambda))
  for (i in 1:length(lambda))
  {functionalPar = fdPar(fdobj=base, Lfdobj=3, lambda=lambda[i])  
  gcv[i] = smooth.basis(abscissa, Xobs0, functionalPar)$gcv
  }
  
  lam=lambda[which.min(gcv)]
  
  functionalPar = fdPar(fdobj=base, Lfdobj=3, lambda=lam)  # functional parameter, having arguments: basis, order of the derivative to be penalized, 
  # smoothing parameter
  
  Xss=smooth.basis(abscissa, Xobs0, functionalPar)
  
  Xss0 = eval.fd(abscissa, Xss$fd, Lfd=0)
  Xss1 = eval.fd(abscissa, Xss$fd, Lfd=1)
  Xss2 = eval.fd(abscissa, Xss$fd, Lfd=2)
  
  df  = Xss$df   #  the degrees of freedom in the smoothing curve
  GCV = Xss$gcv  #  the value of the gcv statistic
  
  if(plot){
    
    x11()
    plot(log10(lambda),gcv)
    
    x11()
    plot(abscissa,Xobs0,xlab="t",ylab="observed data")
    points(abscissa,Xss0 ,type="l",col="blue",lwd=2)
  }
  
  result=list( smoothed_curve=Xss0, der1= Xss1, der2= Xss2, GCV=GCV, df=df,
               order=order, grid_coef=grid_coef);
  result
}
