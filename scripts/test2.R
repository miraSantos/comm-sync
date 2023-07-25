library("wsyn")

times1 <- seq(0,300,1)
times2 <- seq(301,1000,1)
times <- c(times1,times2)
amp = 10
phase_shift = 0
period1 = 100
period2 = 50

ts1<- amp*sin(2*pi/period1*(times-phase_shift))
ts2 <- amp*sin(2*pi/period2*(times-phase_shift))

plot(times,ts1)
ts<-cleandat(ts1,times,clev=1)
wtres<-wt(ts$cdat,times)
class(wtres)
plotmag(wtres)

div = 0
dat<-matrix(NA,2,length(times))
for (counter in 1:dim(dat)[1])
{
   noise <- rnorm(length(times),0,0.2)
   if (counter < div){
   dat[counter,]<-ts1+noise
   }
   if (counter>=div){
     dat[counter,]<- ts2+noise
   }
}

###################################
dat<-cleandat(dat = dat,times = times,clev=1)$cdat
plot(times,dat[1,],type="l",col="red")
lines(times,dat[2,],type="l",col="blue")


###############################################
res<-wpmf(dat,times,sigmethod="quick")

#########################################
jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
               "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
colorfill<-grDevices::colorRampPalette(jetcolors)

zwav = Mod(res$values)

inds<-which(!is.na(colMeans(zwav,na.rm=T)))
zwav<-zwav[,inds]
timescales<-res$timescales[inds]
ylocs <- pretty(timescales, n = 8)
options(repr.plot.width = 15, repr.plot.height =5)

#####################################################################
image.plot(x=times,y=timescales,z=zwav,
           xlab="Time",ylab="Timescale",main=paste(title),
            zlim=c(0,1),
           col=colorfill(100),
           log="y",axes=T)
box()
contour(x=times,y=timescales,z=zwav,add=T,levels=q,labels=c("95%","99%"),drawlabels = T)
# contour(x=df_freq$date,y=timescales,z=zwav,method="edge",add=T,log="y",axes=F,levels=q,drawlabels=T,lwd=2)

