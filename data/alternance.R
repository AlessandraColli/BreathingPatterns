### script to produce alternance.txt data ###

source('../src/breaths_generation.R')

#### asymmetric breaths belonging to two clusters

set.seed(8922961)

per1=1.5
per2=2
per3=1.5
per4=1.5

amp1=1.5
amp2=1.5
amp3=1.25
amp4=1.25

seq1=breaths_seq_gen(25,n_parts=2, sampling_freq=60, 
                     amplitudes=c(amp1,amp2), periods=c(per1,per2), 
                     sd_amp=0.025, sd_per=0.025)

seq2=breaths_seq_gen(25,n_parts=2, sampling_freq=60, 
                     amplitudes=c(amp3,amp4), periods=c(per3,per4), 
                     sd_amp=0.025, sd_per=0.025)

data=c(seq1$y, seq2$y-seq2$y[1]+seq1$y[length(seq1$y)])

seq3=breaths_seq_gen(25,n_parts=2, sampling_freq=60, 
                     amplitudes=c(amp1,amp2), periods=c(per1,per2), 
                     sd_amp=0.025, sd_per=0.025)

data=c(data, seq3$y-seq3$y[1]+data[length(data)])

seq4=breaths_seq_gen(25,n_parts=2, sampling_freq=60, 
                     amplitudes=c(amp3,amp4), periods=c(per3,per4), 
                     sd_amp=0.025, sd_per=0.025)

data=c(data, seq4$y-seq4$y[1]+data[length(data)])

x11()
plot(data, type='l')

# write.table(data, 'alternance.txt')
