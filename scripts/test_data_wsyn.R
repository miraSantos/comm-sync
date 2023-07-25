library("wsyn")
set.seed(1)
times1<-0:50
times2<-51:100
times<-c(times1,times2)
ts1<-3*c(sin(2*pi*times1/10),sin(2*pi*times2/5))+10
ts1<-c(sin(times))

dat<-matrix(NA,11,length(times))
for (counter in 1:dim(dat)[1])
{
  ts2<-3*sin(2*pi*times/3+2*pi*runif(1))+5.0 #random signal
  ts3<- #noise
  dat[counter,]<-ts1+ts3
}
dat<-cleandat(dat,times,1)$cdat


plot(times,dat[1,]/10+1,type='l',xlab="Time",ylab="Time series index",ylim=c(0,12))
for (counter in 2:dim(dat)[1])
{
  lines(times,dat[counter,]/10+counter)
}

res<-wpmf(dat=dat,times=times,sigmethod="quick")
plotmag(res)