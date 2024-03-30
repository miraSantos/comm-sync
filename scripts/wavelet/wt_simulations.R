basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices",
                      "astsa")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
source(paste0(basepath,"/scripts/wavelet/plot_wt_simulations.R"))
#######################################################################################
#SIMULATION SIN CURVE WITH 1 SPIKE
#######################################################################################
sim_x = seq(1,5000,1)
period = 365
noise = rnorm(length(sim_x),mean=0,sd=0.4)
amp = 1
sim_y = amp*sin((2*pi/period)*sim_x) + 10 + noise
sim_y[c(599:604)] = 100

plot(sim_x,sim_y,type="l")
plot(sim_x,sim_y^(1/4))

dat = as.matrix(cbind(sim_x,sim_y^(1/4)))
res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res)

plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_spike_sin_paul.png")

res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res,plot.cb = T)

plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_spike_sin_morlet.png")


#######################################################################################
#SIMULATION SIN CURVE WITH 1 SPIKE
#######################################################################################
sim_x = seq(1,5000,1)
period = 365
noise = rnorm(length(sim_x),mean=0,sd=0.4)
sim_y = sin((2*pi/period)*sim_x) + 5 + noise
sim_y[c(599:604)] = 200

plot(sim_x,sim_y^(1/4),type="l")

dat = as.matrix(cbind(sim_x,sim_y^(1/4)))
res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res,plot.cb = T)

plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_spike_sin_paul.png")

res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res,plot.cb = T)

plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_spike_sin_morlet.png")






#######################################################################################
#SIMULATION SIN - NOISE
#######################################################################################
x = seq(1,5000,1)
period = 365
noise = rnorm(5000,mean=0,sd=0.05)
y1 = sin((2*pi/period)*x) + 5 + noise
y2 = runif(length(x),min = 4,max=6)
y = append(y1[1:2500],y2[2501:5000])
plot(x,y)
dat = as.matrix(cbind(x,scale(y)))

res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res)

plot.wt.simulation(res,x,y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_sin_noise_paul.png")


res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res)

plot.wt.simulation(res,x,y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_sin_noise_morlet.png")


#######################################################################################
#SIMULATION SMALL AMP TO LARGE AMP
#######################################################################################
x = seq(1,5000,1)
period = 365

noise1 = rnorm(length(x),mean=0,sd=0.08)
amp1= 10
y1 = amp1*sin((2*pi/period)*x) + 5 + noise1

noise2 = rnorm(length(x),mean=0,sd=0.04)
amp2 = 100
y2 = amp2*sin((2*pi/period)*x) + 5 + noise2

y = pmax(append(y1[1:2500],y2[2501:5000]),0)
plot(x,y^(1/4))
dat = as.matrix(cbind(x,y^(1/4)))

res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res)

res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res)


plot.wt.simulation(res,x,y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_change_amp_morlet.png")



#######################################################################################
#SIMULATION: TRANSLATION UP
#######################################################################################
x = seq(1,5000,1)
period = 365
translation = 5
amp1 = 2
amp2 = 2
noise = rnorm(length(x),mean=0,sd=0.1)
y1 = amp1*sin((2*pi/period)*x) + noise + 5
y2 = amp2*sin((2*pi/period)*x) + translation + noise +5
split = 2000
y = append(y1[1:split],y2[(split+1):length(x)])
plot(x,y)
plot(x,y^(1/4),type="l")

dat = as.matrix(cbind(x,y^(1/4)))

res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res,plot.cb=T)

plot.wt.simulation(res,x,y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_translate_noise_morlet.png")

res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res)

plot.wt.simulation(res,x,y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_translate_noise_paul.png")



#######################################################################################
#SIMULATION TWO PERIODS COMBINED
#######################################################################################


sim_x = seq(1,5000,1)
period_1 = 16
sim_y_1 = sin((2*pi/period_1)*sim_x) + 5
period_2= 365
sim_y_2 =  sin((2*pi/period_2)*sim_x) + 5

sim_y = sim_y_1 + sim_y_2
plot(sim_x,sim_y,type="l")

dat = as.matrix(cbind(sim_x,sim_y^(1/4)))

res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res)


plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_spike_sin_morlet.png")


res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res)

plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_spike_sin_paul.png")



#######################################################################################
#SIMULATION SLOW AMP CHANGE UP
#######################################################################################
amp_change = seq(1,10,length.out=5000)
sim_x = seq(1,5000,1)
period = 365
noise = rnorm(5000,mean=0,sd=0.1)
sim_y = amp_change*sin((2*pi/period)*sim_x) + 10 + noise

plot(sim_x,sim_y^(1/4),type="l")

dat = as.matrix(cbind(sim_x,sim_y^(1/4)))

res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res,plot.cb = T)

plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_slow_amp_change_morlet.png")


res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res,plot.cb = T)


plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_slow_amp_change_paul.png")



#######################################################################################
#SIMULATION SLOW AMP CHANGE UP - PMAX
#######################################################################################
amp_change = seq(1,15,length.out=5000)
sim_x = seq(1,5000,1)
period = 365
noise = rnorm(5000,mean=0,sd=0.4)
sim_y = pmax(amp_change*sin((2*pi/period)*sim_x) + noise,0)

plot(sim_x,sim_y^1/4,type="l")

dat = as.matrix(cbind(sim_x,sim_y^(1/4)))
res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res,plot.cb = T)

res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res,plot.cb = T)

plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_slow_amp_change_paul.png")



#######################################################################################
#SIMULATION PMAX 
#######################################################################################
amp_change = seq(1,10,length.out=5000)
sim_x = seq(1,5000,1)
period = 365
noise = rnorm(5000,mean=0,sd=0.4)
sim_y = pmax(10*sin((2*pi/period)*sim_x) + noise,0)

plot(sim_x,sim_y,type="l")

dat = as.matrix(cbind(sim_x,scale(sim_y)))
res= wt_arc(dat,mother="morlet")
plot.biwavelet_adv(res,plot.cb = T)

res= wt_arc(dat,mother="paul")
plot.biwavelet_adv(res,plot.cb = T)

plot.wt.simulation(res,sim_x,sim_y,save_folder=paste0(basepath,"/figures/wavelet_transforms/simulations/"),
                   save_name="simulation_slow_amp_change_paul.png")



