# install.packages(c("wsyn","mvtnorm","RColorBrewer","fields","ggplot2"))
library("wsyn")
library("mvtnorm")
library("RColorBrewer")
library("fields")
library("ggplot2")


plotmag <- function(){
  res<-wpmf(dat,times,sigmethod="quick")
  cmat<-cor(t(dat))
  diag(cmat)<-NA
  cmat<-as.vector(cmat)
  cmat<-cmat[!is.na(cmat)]
  hist(cmat,30,xlab="Pearson correlation",ylab="Count")
  
  res<-wpmf(dat,times,sigmethod="quick")
  
  jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
  colorfill<-grDevices::colorRampPalette(jetcolors)
  pal = colorRampPalette(brewer.pal(9, 'Oranges'))(1024)
  
  zwav = Mod(res$values)
  sigthresh = 0.95
  q = stats::quantile(res$signif[[2]],sigthresh)
  
  inds<-which(!is.na(colMeans(zwav,na.rm=T)))
  zwav<-zwav[,inds]
  timescales<-res$timescales[inds]
  ylocs <- pretty(timescales, n = 8)
  options(repr.plot.width = 15, repr.plot.height =5)
  
  sigthresh = 0.95
  q = stats::quantile(res$signif[[2]],sigthresh)
  
  image.plot(x=res$times,y=timescales,z=zwav,
             xlab="Time",ylab="Timescale",
             zlim=c(0,1),
             col=pal,log="y",
             main="Wavelet Mean Phasor Field Magnitude")
  contour(x=res$times,y=timescales,z=zwav,
          add=T,color="blue",
          axes=F,levels=q,drawlabels=F,lwd=2)
  
  sm<-synmat(dat,times,method="pearson")
  fields::image.plot(1:10,1:10,sm,col=pal,zlim=c(0,1),main="Synchrony Matrix")
  
  h<-power(res)
  coh <- data.frame(x = -log(1/h$timescales),y = h$power)
  xlocs<-c(1:3 %o% 10^(1:3))
  ggplot(data = coh,aes(x = x, y = y)) + 
    geom_line()+
    annotation_logticks(sides="b")+
    # scale_x_continuous(breaks = c(-log(1/xlocs),365),labels=c(xlocs,365))+
    xlab("Timescales")+
    ylab("Power")+
    ggtitle(paste("Power"))
}

#generate times
times1<-0:50
times2<-51:200
times<-c(times1,times2)

#create 2 sine wave signals
amp1 = 1
amp2 = 10
phase_shift = pi
period1 = 2*pi
period2 = 2*pi

div = 5 #divide data into two groups after this index

nrow = 10

ts1<-c(amp1*sin((2*pi/period1)*(times)))
ts2<-c(amp2*sin((2*pi/period2)*(times+phase_shift)))

dat<-matrix(NA,nrow,length(times))
for (counter in 1:nrow){
  if (counter < div){dat[counter,]<-ts1}
  if (counter >=div){dat[counter,]<-ts2}}
dat<-cleandat(dat,times,1)$cdat

plot(times,dat[1,]/nrow+1,type='l',xlab="Time",ylab="Time series index",ylim=c(0,12))
for (counter in 2:dim(dat)[1]){
  lines(times,dat[counter,]/nrow+counter)}
plotmag()