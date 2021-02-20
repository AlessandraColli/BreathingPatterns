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
  
  result=list( smoothed_curve=Xss0, der1= Xss1, der2= Xss2, GCV=GCV, df=df);
  result
}
