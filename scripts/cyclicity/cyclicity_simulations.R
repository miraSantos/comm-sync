list.of.packages <- c("dplyr","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#EXPLORE SENSITIVTY TO TIMING VS. AMPLITUDE

#simulate change in timing
num_years = 10
#generate data frame to store weekly simulated data
df=data.frame(x = seq(1,53*num_years,1),week=rep(seq(1,53,1),num_years))
#create year vector to mark each year in the data set
df$year <- rep(seq(1,num_years,1),each=53)
df$baseline <- sin(B*df$x)+10

#set period to weekly
period=53
B=(2*pi)/period

#create signal with varying amplitudes each year
amp_var = seq(10,100000,length.out=num_years)
df$A=rep(amp_var,each=53)
noise=rnorm(length(df$A),mean=0,sd=2)
df$y_amp=df$A*sin(B*df$x)+10+noise

plot(df$x,df$y_amp)

#create signal with varying phase each year
max_shift = 10 #max number weeks to shift
phase_var = -seq(0,max_shift,length.out=num_years) #0 to six week shift
# phase_var = runif(num_years,min=0,max=20) #random shift
df$C = rep(phase_var,each=53)
df$y_phase = sin(B*(df$x+df$C))+10

plot(df$x,df$y_phase)

#create signal with varying noise
noise_var = seq(0,1,length.out=num_years)
df$sd=rep(noise_var,each=53)
df$noise=rnorm(length(df$A),mean=0,sd=df$sd)
df$y_noise=sin(B*df$x)+20+df$noise

plot(df$x,df$y_noise,type="l")

#compute weekly climatology and add to data frame
df_means <- df %>% group_by(week) %>% 
  summarise(y_amp_mean = mean(y_amp),
            y_phase_mean=mean(y_phase),
            y_noise_mean = mean(y_noise))
df$y_phase_mean <- rep(df_means$y_phase_mean,num_years)
df$y_amp_mean <- rep(df_means$y_amp_mean,num_years)
df$y_noise_mean <- rep(df_means$y_noise_mean,num_years)


####################################################
#compute correlation
###############################################################
df_local_c_index <- df %>% group_by(as.factor(year)) %>% 
  summarise(amp_cor= cor(y_amp_mean,y_amp),
            phase_cor=cor(y_phase_mean,y_phase),
            noise_cor=cor(y_noise_mean,y_noise))

df_local_c_index$phase_var = phase_var
df_local_c_index$amp_var = amp_var
df_local_c_index$noise_var= noise_var
df_local_c_index$year <- seq(1,num_years,length.out=num_years)


mean(df_local_c_index$phase_cor)
var(df_local_c_index$phase_cor)

mean(df_local_c_index$amp_cor)
var(df_local_c_index$phase_cor)

mean(df_local_c_index$noise_cor)
var(df_local_c_index$noise_cor)
#######################################################
#Plot change in amplitude
#######################################################
ggplot() + geom_line(data=df,aes(x=week,y=y_amp,color=as.factor(year)),
                     size=1,alpha=0.5)+
  geom_line(data=df_means,aes(x=week,y=y_amp_mean),
            color="black",linetype="dashed",size=1)+
  theme_bw() +
  ggtitle("Amplitude change") +
  labs(color="Year",x="Week of Year",y="Amplitude")

#plot c_index vs. year for amplitude case
df_local_c_index %>% ggplot() +
  geom_point(aes(x=year,y=amp_cor))+ylim(0,1)+
  ggtitle("Local Cyclicity - Amplitude")+
  scale_x_continuous(breaks=seq(1,10,1))+
  labs(x="Year",y="Local Cyclicity")

#plot c_index vs. phase_var
df_local_c_index %>% ggplot() +
  geom_point(aes(x=amp_var,y=amp_cor))+
  ggtitle("Local Cyclicity - Phase Shift")

##############################################################################
#Plot change in timing of bloom
################################################################################
ggplot() + 
  geom_line(data=df,aes(x=week,y=y_phase,color=as.factor(year)),size=1,
                     alpha=0.5)+
  geom_line(data=df_means,aes(x=week,y=y_phase_mean),
            color="black",
            linetype="dashed",size=1)+
  theme_bw()+
  ggtitle("Phase shift") +
  labs(color="Year",x="Week of Year",y="Amplitude")


#plot c_index vs. year for phase shift case
df_local_c_index %>% ggplot() +
  geom_point(aes(x=year,y=phase_cor))+ylim(0,1)+
  ggtitle("Local Cyclicity - Phase Shift")+
  scale_x_continuous(breaks=seq(1,10,1))+
  labs(x="Year",y="Local Cyclicity")

#plot c_index vs. phase_var
df_local_c_index %>% ggplot() +
  geom_point(aes(x=phase_var,y=phase_cor))+
  ggtitle("Local Cyclicity - Phase Shift")

#################################################################################
################################################################################

ggplot() + geom_line(data=df,aes(x=week,y=y_noise,color=as.factor(year)),
                     size=1,alpha=0.5)+
  geom_line(data=df_means,aes(x=week,y=y_noise_mean),
            color="black",linetype="dashed",size=1)+
  theme_bw() +
  ggtitle("Noise change") +
  labs(color="Year",x="Week of Year",y="Amplitude")

#plot c_index vs. year for phase shift case
df_local_c_index %>% ggplot() +
  geom_point(aes(x=year,y=noise_cor))+ylim(0,1)+
  ggtitle("Local Cyclicity - Noise")+
  scale_x_continuous(breaks=seq(1,10,1))+
  labs(x="Year",y="Local Cyclicity")

#plot c_index vs. phase_var
df_local_c_index %>% ggplot() +
  geom_point(aes(x=noise_var,y=noise_cor))+
  ggtitle("Local Cyclicity - Phase Shift")+ylim(0,1)+
  labs(x="Noise Standard Deviation",y="Local Cyclicity")
