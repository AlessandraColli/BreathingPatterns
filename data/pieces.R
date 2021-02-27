### script to produce the components of and assemble perturbed.txt data ###

source('../src/breaths_generation.R')

set.seed(8922961)

# normal quiet breathing

per1=1.5
per2=2

amp1=1
amp2=1

for(i in 1:10){
  
seq=breaths_seq_gen(nbreaths=10, n_parts=2, sampling_freq=60, 
                     amplitudes=c(amp1,amp2), periods=c(per1,per2), 
                     sd_amp=0.05, sd_per=0.1)

write.table(seq$y, file=paste0("normal",i,".txt"))

}

# irregular

per1=1
per2=1
per3=0.5
per4=1.5

amp1=1
amp2=0.5
amp3=0.05
amp4=0.55

seq=breaths_seq_gen(nbreaths=5, n_parts=4, sampling_freq=60, 
                    amplitudes=c(amp1,amp2,amp3,amp4), periods=c(per1,per2,per3,per4), 
                    sd_amp=0.1, sd_per=0.2)
plot(seq$y)

write.table(seq$y, file='irregular.txt')


# cough-like

per1=1
per2=1
per3=1
per4=1

amp1=1
amp2=3
amp3=3
amp4=3

conn1=breaths_seq_gen(nbreaths=1, n_parts=4, sampling_freq=60, 
                      amplitudes=c(amp1,amp2,amp3,amp4), periods=c(per1,per2,per3,per4), 
                      sd_amp=0.1, sd_per=0.1)

plot(conn1$y)

seq=breaths_seq_gen(nbreaths=2, n_parts=2, sampling_freq=60, 
                        amplitudes=c(amp3,amp4), periods=c(per3,per4), 
                        sd_amp=0.1, sd_per=0.1)

plot(c(conn1$y,seq$y-seq$y[1]+conn1$y[length(conn1$y)]))

y=c(conn1$y,seq$y-seq$y[1]+conn1$y[length(conn1$y)])

conn2=breaths_seq_gen(nbreaths=1, n_parts=4, sampling_freq=60, 
                      amplitudes=c(amp4,amp3,amp2,amp1), periods=c(per4,per3,per2,per1), 
                      sd_amp=0.1, sd_per=0.1)

plot(conn2$y)

y=c(y, conn2$y-conn2$y[1]+y[length(y)])
plot(y)

write.table(y, file='cough.txt')

# deep breaths

per1=3
per2=3

amp1=4
amp2=4

seq=breaths_seq_gen(nbreaths=5, n_parts=2, sampling_freq=60, 
                    amplitudes=c(amp1,amp2), periods=c(per1,per2), 
                    sd_amp=0.1, sd_per=0.2)

plot(seq$y)

write.table(seq$y, file='deep.txt')

# long inspiration

per1=3
per2=2

amp1=1.25
amp2=1.25

seq=breaths_seq_gen(nbreaths=5, n_parts=2, sampling_freq=60, 
                    amplitudes=c(amp1,amp2), periods=c(per1,per2), 
                    sd_amp=0.1, sd_per=0.2)

plot(seq$y)

write.table(seq$y, file='longinsp.txt')

# long expiration

per1=1.5
per2=3

amp1=1.25
amp2=1.25

seq=breaths_seq_gen(nbreaths=5, n_parts=2, sampling_freq=60, 
                    amplitudes=c(amp1,amp2), periods=c(per1,per2), 
                    sd_amp=0.1, sd_per=0.2)

plot(seq$y)

write.table(seq$y, file='longexp.txt')

# magnitude1

per1=1
per2=1.5
per3=1
per4=1.5

amp1=2
amp2=1
amp3=1
amp3=2

seq=breaths_seq_gen(nbreaths=3, n_parts=4, sampling_freq=60, 
                    amplitudes=c(amp1,amp2,amp3,amp4), periods=c(per1,per2,per3,per4), 
                    sd_amp=0.1, sd_per=0.5)

plot(seq$y)

write.table(seq$y, file='chaos.txt')

# magnitude outliers 2

per1=1.5
per2=2

amp1=2.5
amp2=2.5

seq=breaths_seq_gen(nbreaths=5, n_parts=2, sampling_freq=60, 
                    amplitudes=c(amp1,amp2), periods=c(per1,per2), 
                    sd_amp=0.05, sd_per=0.2)

plot(seq$y)

write.table(seq$y, file='magnitude.txt')

# shape

per1=1
per2=0.5
per3=0.5
per4=1.5

amp1=1
amp2=0.15
amp3=0.15
amp4=1

seq=breaths_seq_gen(nbreaths=5, n_parts=4, sampling_freq=60, 
                    amplitudes=c(amp1,amp2,amp3,amp4), periods=c(per1,per2,per3,per4), 
                    sd_amp=0.1, sd_per=0.2)
plot(seq$y)

write.table(seq$y, file='irregular2.txt')

# pauses

per1=2
per2=2
per3=1
per4=1

amp1=1
amp2=1
amp3=0.025
amp4=0.025

seq=breaths_seq_gen(nbreaths=5, n_parts=4, sampling_freq=60, 
                    amplitudes=c(amp1,amp2,amp3,amp4), periods=c(per1,per2,per3,per4), 
                    sd_amp=0.0025, sd_per=0.2)
plot(seq$y)

write.table(seq$y, file='pauses.txt')

# visualization

data=read.table('normal1.txt',header=T)[,1]

y=read.table('irregular.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal2.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('cough.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal3.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('deep.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal4.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('longinsp.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal5.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('chaos.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal6.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('longexp.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal7.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('magnitude.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal8.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('irregular2.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal9.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('pauses.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

y=read.table('normal10.txt', header=T)[,1]

data=c(data,y-y[1]+data[length(data)])

time=(0:(length(data)-1))/60

x11()
plot(time, data, col='black', type = 'l', main='CW volume', xlab='time (s)', ylab='volume (L)')

write.table(data, file='perturbed.txt')
