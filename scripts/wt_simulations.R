x = seq(1,1000,1)
y = sin(x) + 5
y[c(599,600,601)] = 100
dat = as.matrix(cbind(x,scale(y)))
res= wt_arc(dat)
plot.biwavelet_adv(res,plot.cb=T)

plot(x,y,type="l")

x1 = seq(1,1000,1)
y1= sin(x) + 5
y1[c(599,600,601)] = 60
dat1 = as.matrix(cbind(x1,log(y1)))
res1= wt_arc(dat1)
plot.biwavelet_adv(res1,plot.cb=T)

x2 = seq(1,1000,1)
y2 = sin(x) + 5
dat2 = as.matrix(cbind(x2,log(y2)))
res2= wt_arc(dat2)
plot.biwavelet_adv(res2,plot.cb=T)

