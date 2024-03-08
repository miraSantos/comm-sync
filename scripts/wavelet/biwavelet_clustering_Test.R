list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
#checking for new packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#installing new packages
if(length(new.packages)) install.packages(new.packages)
#loading packages
lapply(list.of.packages, require, character.only = TRUE)
source("/home/mira/MIT-WHOI/community_sychrony/scripts/adv_biwavelet_packages.R")
rm(wt) #replaces wt with the correct biwavelet version (there are 2 versions)

#GOAL: make synthetic data to explore clustering results.
#SCENARIO 1: 6 synchronous signals with different frequencies

#create 2 clusters: 2 synchronous clusters at 2 sets of frequencies

period = 32
noise = 0.4
phase_shift = 0
x = seq(0,365,1)
period_shift= seq(1,10,length.out=length(x))

# Anti-correlated ---------------------------------------------------------
phase_shift= period/2
sig1 = sin(2*pi/period*x) + rnorm(length(x),mean=0,sd=noise)
sig2 = sin(2*pi/period*(x+phase_shift))+ rnorm(length(sig1),mean=0,sd=noise)

plot(x,sig1,type="solid",
     xlab = "Time",
     ylab="Signal")
lines(x,sig2,col="red")

dat1 = as.matrix(cbind(x,sig1))
dat2 = as.matrix(cbind(x,sig2))
res = wtc_arc(dat1,dat2,
              s0=10,
              asig.level=c(0.99),
              psig.level=c(0.99),anrands=500)

w.arr <- array(dim = c(length(wt_list), nrows, ncols))

