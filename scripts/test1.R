library("wsyn")
library(ggplot2)

times <- seq(1,100,1)

phase_shift = 0 
amp = 1
period= 20
ts1<-amp*sin(2*pi/period*(times))
dev.off()
plot(times,ts1,type="b")

dat<-matrix(NA,11,length(times))

for (counter in 1:dim(dat)[1]){
  dat[counter,] <- ts1
}

dat <- cleandat(dat,times,1)$cdat

plot(times,dat[1,]/10+1,type='l',xlab="Time",ylab="Time series index",ylim=c(0,12))
for (counter in 2:dim(dat)[1])
{
  lines(times,dat[counter,]/10+counter)
  
res<-wpmf(dat,times,sigmethod="quick")
plotmag(res)
  
}