### script to produce veryregular.txt and regular.txt data ###

source('../src/breaths_generation.R')

#### symmetric curves, all with the same mean amplitude and period

set.seed(8922961)

per=3.5
amp=1

seq=breaths_seq_gen(nbreaths=100, n_parts=1, sampling_freq=60, 
                    amplitudes=amp, periods=per,
                    sd_amp=0.025, sd_per=0.05)

x11()
plot(seq$y, type='l')

# write.table(seq$y, file='veryregular.txt')

#### asymmetric curves with the same parameters

set.seed(8922961)

per1=1.5
per2=2

amp1=1
amp2=1

seq=breaths_seq_gen(nbreaths=100, n_parts=2, sampling_freq=60,
                    amplitudes=c(amp1,amp2), periods=c(per1,per2),
                    sd_amp=0.025, sd_per=0.05)

x11()
plot(seq$y, type='l')

# write.table(seq$y, file='regular.txt')
