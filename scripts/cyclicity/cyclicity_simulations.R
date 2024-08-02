list.of.packages <- c("dplyr","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))
#EXPLORE SENSITIVTY TO TIMING VS. AMPLITUDE

#simulate change in timing
num_years = 10
#set period to weekly
period=53*2
B=(2*pi)/period
#generate data frame to store weekly simulated data
df=data.frame(x = seq(1,53*num_years,1),week=rep(seq(1,53,1),num_years))
#create year vector to mark each year in the data set
df$year <- rep(seq(1,num_years,1),each=53)
df$baseline <- sin(B*df$x)+10

#create signal with varying amplitudes each year
amp_var = seq(100,10000,length.out=num_years)
df$A=rep(amp_var,each=53)
noise=rnorm(length(df$A),mean=0,sd=100)
df$y_amp=df$A*sin(B*df$x)+10+noise

plot(df$x,abs(df$y_amp))

#create signal with varying phase each year
max_shift = 20 #max number weeks to shift
phase_var = -seq(0,max_shift,length.out=num_years) #0 to six week shift
# phase_var = runif(num_years,min=0,max=20) #random shift
df$C = rep(phase_var,each=53)
period=53
B=(2*pi)/period
noise_y_phase=rnorm(length(df$A),mean=0,sd=0.5)
df$y_phase = 10*sin(B*(df$x+df$C))+20+noise_y_phase

mean_df = df %>% group_by(week) %>% summarise(mean = mean(y_phase,na.rm=T))
plot(df$x,df$y_phase)
plot(df$week,df$y_phase,col=df$year)
lines(mean_df$week,mean_df$mean,col="red")

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
ggplot() + geom_line(data=df,aes(x=week,y=abs(y_amp),color=as.factor(year)),
                     size=1,alpha=0.5)+
  geom_line(data=df_means,aes(x=week,y=y_amp_mean),
            color="black",linetype="dashed",size=1)+
  scale_y_log10()+
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
  geom_line(data=df,aes(x=week,y=y_phase+10000,color=as.factor(year)),size=1,
                     alpha=0.5)+
  theme_bw()+
  scale_y_log10()+
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


################################################################################
################################################################################

head(df)
years=seq(1,10,1)
lower_lim=-10
upper_lim=10
maxit=1000

shifts_year <- data.frame(year=rep(years,2),lag=0,
                          lag_type=c(rep("time_lag",10),rep("amp_lag",10)))

df_log <- df %>% mutate(y_amp=log10(abs(y_amp)+0.1),y_phase=log10(y_phase+0.1))
plot(df_log$week,df_log$y_phase,col=df_log$year)

RSS_optim_year_null <- psoptim(par=shifts_year$lag,
                          fn=RSS,df=df_log,
                          taxa="y_phase",
                          shifts=shifts_year,
                          unit_j = "year",
                          fix_t=T,
                          lower=rep(lower_lim,length(years)),
                          upper=rep(upper_lim,length(years)),
                          control=list(maxit=maxit))

RSS_optim_year <- psoptim(par=shifts_year$lag,
                               fn=RSS,df=df_log,
                               taxa="y_phase",
                               shifts=shifts_year,
                               unit_j = "year",
                               fix_t=F,
                               lower=rep(lower_lim,length(years)),
                               upper=rep(upper_lim,length(years)),
                               control=list(maxit=maxit))



shift_opt_null = retrieve_lags(RSS_optim_year_null,shifts_year,fix_t=T)
shift_opt = retrieve_lags(RSS_optim_year,shifts_year,fix_t=F)

plot(shift_opt_null$shift$year,shift_opt_null$shift$amp_lag)
plot(shift_opt_null$shift$year,shift_opt_null$shift$time_lag)

#Unconstrained
plot(shift_opt$shift$year,shift_opt$shift$amp_lag)
plot(shift_opt$shift$year,shift_opt$shift$time_lag)

s_mean_null <- seasonal_mean(par=RSS_optim_year_null$par,
              df=df_log,
              taxa="y_phase",
              shifts=shifts_year,
              unit_j="year",
              fix_t=T)
s_mean <- seasonal_mean(par=RSS_optim_year$par,
                             df=df_log,
                             taxa="y_phase",
                             shifts=shifts_year,
                             unit_j="year",
                             fix_t=F)

plot(s_mean_null$week_shifted,s_mean_null$y_phase)
lines(s_mean$week_shifted,s_mean$y_phase)

df_log$t = index(df_log)
taxa = "y_phase"
df=df_log
shifts_wide = shifts_year %>% pivot_wider(names_from=lag_type,values_from=lag)
shifts_wide$time_lag <- shift_opt$time_lags$lag
shifts_wide$amp_lag <- shift_opt$amp_lags$lag
#join seasonal shifts to dataframe indices
df_shifts <- left_join(df[,c(unit_j,"week","t",taxa)],shifts_wide,by=unit_j) %>%
  #compute new shifts
  mutate(week_shifted = (week - time_lag)) %>% mutate(week_shifted = case_when(week_shifted<1~NaN,
                                                                                   week_shifted>53~NaN,
                                                                               week_shifted %in% seq(1,53,1)~week_shifted))
#apply shifts to dataframe
var_shifted = df[,taxa]-df_shifts$amp_lag 
#compute new mean annual cycle
sub_lag <- function(x){return(x-df_shifts$amp_lag)}
mean_adjusted = df_shifts %>%
  group_by(week_shifted) %>% summarise_at(taxa,mean,na.rm=T)

unadjusted = df %>% mutate_at(taxa,sub_lag) %>%
  group_by(week) %>% summarise_at(taxa,mean,na.rm=T)


ggplot() + geom_line(data=mean_adjusted,aes(x=week_shifted,y=y_phase,color="Adjusted Seasonal Mean"),linetype="solid",linewidth=2) + 
  geom_line(data=unadjusted,aes(x=week,y=y_phase,color="Seasonal Mean Null Model"),linetype="dashed",linewidth=2) +
  geom_line(data=df,aes(x=week,y=y_phase,fill=as.factor(year),color="#RRGGBBAA" ))+
  theme_bw()+
  scale_color_manual(values=c("Adjusted Seasonal Mean" = "green","Seasonal Mean Null Model"="red"))

ggplot() + geom_line(data=mean_adjusted,aes(x=week_shifted,y=y_phase),color="green")
 #######################################################################################################################



lag_length = 136 #length of lag vector (all time and amp lags)
#applying the statistic RSS_0 - RSS_1
rss_1_lag = RSS_optim_year$par
#set taxa
#compute statistic of original time series
orig.ts.stat = shift.statistic(error=shift.res)
#number of replicates
num_replicates = 10000

#function to compute statistic
shift.statistic <- function(error){
  df_mean_a$sim = df_mean_a$amp_lag + df_mean_a$seasonal_mean + error
  time_lag_i = length(shifts$year)/2
  #round time lags to integers
  rss_0_lag = RSS_optim_year$par
  rss_0_lag[1:time_lag_i] = round(par[1:time_lag_i])
  rss_1_lag = RSS_optim_year$par
  #RSS assuming no lag
  RSS_0 = RSS(par = rss_0_lag ,df=df_mean_a,
              taxa=taxa,
              shifts=shifts_year,unit_j="year",fix_t=T)
  #RSS with unconstrained lag
  RSS_1 = RSS(par= rss_1_lag,df=df_mean_a,
              taxa=taxa,
              shifts=shifts_year,unit_j="year",fix_t=F)
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}

#time series
ts.length = length(df_mean_a$year)

#retrieve ar coefficients
#generate model with ar coefficients

#compute residuals
shift.res <- (df_mean_a %>% mutate(error=y_phase-seasonal_mean-amp_lag))$error


#function to generate simulated data
shift.sim <- function(res,n.sim, ran.args) {
  # random generation of replicate series using arima.sim 
  rg1 <- function(n, res) sample(res, n, replace = TRUE)
  return(df_mean_a$amp_lag + df_mean_a$seasonal_mean+rg1)
}

###################################
#bootstrap
###################################
shift.boot <- tsboot(shift.res,
                     shift.statistic,
                     R = num_replicates, #bootstrap replicates required
                     sim = "scramble",
                     n.sim = ts.length, #length of simulated time series
                     orig.t = F,
                     ran.gen = shift.sim, 
                     ran.args = list())

#p-value
###proportion of values of the statistic that exceed the original
num.greater= length(which(shift.boot$t > orig.ts.stat))

###p-val is fraction of bootstrapped numbers that exceed the original
p.val = num.greater/length(shift.boot$t)
hist(shift.boot$t)
print(paste("p.val=",p.val))