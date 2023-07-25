library("wsyn")


times1 <- seq(0,300,1)
times2 <- seq(301,1000,1)
times <- c(times1,times2)
amp = 5
phase_shift = 0

period1 = 100
period2 = 50

ts1<- amp*sin(2*pi/period1*(times-phase_shift))
ts2 <- amp*sin(2*pi/period2*(times-phase_shift))

#########################
#WAVELET TRANSFORM

plot(times,ts1)
ts<-cleandat(ts1,times,clev=1)
wtres<-wt(ts$cdat,times)
class(wtres)
plotmag(wtres)


############################################
#Wavlet Phasor Mean Field
div = 1
num_groups= 2
dat<-matrix(NA,num_groups,length(times))
for (counter in 1:dim(dat)[1])
{
   noise <- rnorm(length(times),0,1)
   if (counter < div){
   dat[counter,]<-ts1+noise
   }
   if (counter>=div){
     dat[counter,]<- ts2+noise
   }
}

str(dat)
dat<-cleandat(dat = dat,times = times,clev=1)$cdat

##################################
######plot data
plot(times,dat[1,],type="l",col = "red",lwd=2)
lines(times,dat[2,],type="l",col="blue",lwd=2)

####################################
##### plot wpmf
res<-wpmf(dat,times,sigmethod="quick")
plotmag(res)
