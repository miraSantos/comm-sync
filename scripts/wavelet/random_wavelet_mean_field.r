# install.packages(c("wsyn","mvtnorm","RColorBrewer","fields","ggplot2"))
library("wsyn")
library("mvtnorm")
library("RColorBrewer")
library("fields")
library("ggplot2")


plotmag <- function(sigmethod="quick"){
  res<-wpmf(dat,times,sigmethod=sigmethod)
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

times1<-0:50
times2<-51:100
times<-c(times1,times2)

num_spec = 10


dat <- matrix(runif(10*length(times),min=0,max=2),nrow=10) #generate matrix of random numbers sampled from uniform distribution.
dat<-cleandat(dat,times,1)$cdat

plot(times,dat[1,]/num_spec+1,type='l',xlab="Time",ylab="Time series index",ylim=c(0,12))
for (counter in 2:dim(dat)[1]){
  lines(times,dat[counter,]/num_spec+counter)}
plotmag(sigmethod="quick")