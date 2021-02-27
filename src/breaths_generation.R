rescale_time=function(x,y,n=1000){      
  
  # step=length(x)/n
  f=x[1];
  t=x[length(x)];
  
  newx=seq(f,t,length.out=n);
  
  res_curve=spline(x,y,xout=newx);
  res_curve$y
}

breath_gen=function(n_parts=1, sampling_freq=60, amplitudes=2, periods=3, sd_amp=0.05, sd_per=0.1){
  
  la=length(amplitudes)
  lp=length(periods)
  
  delta_amp=rnorm(la, 0, sd_amp)
  delta_per=rnorm(lp, 0, sd_per)
  
  per=(periods+delta_per)
  amp=(amplitudes+delta_amp)
  
  if(n_parts==1){                           # respiro simmetrico: un solo seno

    x=c(seq(-pi/2*per, 3/2*pi*per, by=0.01),3/2*pi*per) 
    
    y=amp*sin(x/per) + amp
    
  
  }else if(n_parts==2){          # respiro asimmetrico
    
    x1=c(seq(-pi/2*per[1], 1/2*pi*per[1], by=0.01),1/2*pi*per[1]) 
    
    y1=amp[1]*sin(x1/per[1]) + amp[1]
    
    x2=c(seq(pi/2*per[2], 3/2*pi*per[2], by=0.01),3/2*pi*per[2]) 
    
    y2=amp[2]*sin(x2/per[2])
    y2 = y2 -y2[1] + y1[length(y1)]
    
    y=c(y1,y2)
    
      
  }else if(n_parts==3){        # S rovesciata : connettore tra parti
    
    x1=c(seq(-pi/2*per[1], 1/2*pi*per[1], by=0.01),1/2*pi*per[1]) 
    
    y1=amp[1]*sin(x1/per[1]) + amp[1]
    
    x2=c(seq(pi/2*per[2], 3/2*pi*per[2], by=0.01),3/2*pi*per[2]) 
    
    y2=amp[2]*sin(x2/per[2])
    y2 = y2 -y2[1] + y1[length(y1)]
    
    x3=c(seq(-pi/2*per[3], 1/2*pi*per[3], by=0.01),1/2*pi*per[3]) 
    
    y3=amp[3]*sin(x3/per[3]) 
    y3 = y3 -y3[1] + y2[length(y2)]
    
    y=c(y1,y2,y3)
    
    
  }else if(n_parts==4){         # capacità vitale o due gobbe
    
    x1=c(seq(-pi/2*per[1], 1/2*pi*per[1], by=0.01),1/2*pi*per[1]) 
    
    y1=amp[1]*sin(x1/per[1]) + amp[1]
    
    x2=c(seq(pi/2*per[2], 3/2*pi*per[2], by=0.01),3/2*pi*per[2]) 
    
    y2=amp[2]*sin(x2/per[2])
    y2 = y2 -y2[1] + y1[length(y1)]
    
    x3=c(seq(-pi/2*per[3], 1/2*pi*per[3], by=0.01),1/2*pi*per[3]) 
    
    y3=amp[3]*sin(x3/per[3]) 
    y3 = y3 -y3[1] + y2[length(y2)]
    
    x4=c(seq(pi/2*per[4], 3/2*pi*per[4], by=0.01),3/2*pi*per[4]) 
    
    y4=amp[4]*sin(x4/per[4])                    
    y4 = y4 -y4[1] + y3[length(y3)]
    
    y=c(y1,y2,y3,y4)
    
  }
  
  duration =sum(per)

  npoints=floor(duration*sampling_freq) #60hz


  signal=rescale_time(1:length(y),y,n=npoints)
  
  res=list(y=signal, lengths=per, amp=amp)
  
  res
  
}

breaths_seq_gen=function(nbreaths=1, n_parts=1, sampling_freq=60, amplitudes=2, periods=3, sd_amp=0.05, sd_per=0.1){
  
  y=c()
  lengths=c()
  amp=c()
  
  br=breath_gen(n_parts=n_parts,sampling_freq=sampling_freq, amplitudes=amplitudes, 
                 periods=periods, sd_amp=sd_amp, sd_per=sd_per)
  y=c(y, br$y)
  lengths=c(lengths, br$lengths)
  amp=c(amp, br$amp)

  if(nbreaths>1){
    
    for(i in 1:(nbreaths-1)){
      br=breath_gen(n_parts=n_parts,sampling_freq=sampling_freq, amplitudes=amplitudes, 
                     periods=periods, sd_amp=sd_amp, sd_per=sd_per)
      y=c(y, br$y-br$y[1]+y[length(y)])
      lengths=c(lengths, br$lengths)
      amp=c(amp, br$amp)
    }
  }
  
  res=list(y=y, lengths=lengths, amp=amp)
}
