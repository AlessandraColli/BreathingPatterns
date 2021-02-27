### script to produce chaotic.txt data ###

source('../src/breaths_generation.R')

#### couples of asymmetric breaths with high param variance

set.seed(8922961)

per1=2
per2=1
per3=0.2
per4=1

amp1=1.5
amp2=1.5
amp3=0.25
amp4=0.25

seq=breaths_seq_gen(nbreaths=100, n_parts=4, sampling_freq=60, 
                    amplitudes=c(amp1,amp2,amp3,amp4), periods=c(per1,per2,per3,per4), 
                    sd_amp=0.5, sd_per=0.05)

x11()
plot(seq$y, type='l')

# write.table(seq$y, file='chaotic.txt')
