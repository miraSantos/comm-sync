library("wsyn")

x = seq(1,200,1)
amp=1
period=15
y = sin(2*pi/period*x)
noise1 = rnorm(length(y),0,0.4)
noise2= rnorm(length(y),0,0.4)
dat = rbind(y+noise1,y+noise2)
times = x

dat <- cleandat(dat,times,1)$cdat
res <- wpmf(dat,times)
plotmag(res)


wavarray<-warray(dat, times, scale.min, scale.max.input, sigma, f0)
wmf <- apply(wavarray$wavarray,c(2,3),mean,na.rm=T)
timescales<-wavarray$timescales
wavarray<-wavarray$wavarray


