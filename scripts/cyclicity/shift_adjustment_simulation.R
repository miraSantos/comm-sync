
#simulate change in timing
num_years = 10
#generate data frame to store weekly simulated data
df=data.frame(x = seq(1,53*num_years,1),week=rep(seq(1,53,1),num_years))
#create year vector to mark each year in the data set
df$year <- rep(seq(1,num_years,1),each=53)

#set period to weekly
period=53*2
B=(2*pi)/period
df$baseline <- sin(B*df$x)+10

#create signal with varying amplitudes each year
amp_var = seq(100,10000,length.out=num_years)
df$A=rep(amp_var,each=53)
noise=rnorm(length(df$A),mean=0,sd=2)
df$y_amp=df$A*sin(B*df$x)+10+noise

plot(df$x,abs(df$y_amp))

#create signal with varying phase each year
max_shift = 10 #max number weeks to shift
phase_var = -seq(0,max_shift,length.out=num_years) #0 to six week shift
# phase_var = runif(num_years,min=0,max=20) #random shift
period=53
B=(2*pi)/period
df$C = rep(phase_var,each=53)
df$y_phase = sin(B*(df$x+df$C))+10

plot(df$x,df$y_phase)
plot(df$week,df$y_phase)

#create signal with varying noise
noise_var = seq(0,1,length.out=num_years)
df$sd=rep(noise_var,each=53)
df$noise=rnorm(length(df$A),mean=0,sd=df$sd)
df$y_noise=sin(B*df$x)+20+df$noise

#APPLY TO SIMULATED DATA
df$t = df$x
num_years = max(df$year)
shifts_simulation <- data.frame(year=rep(seq(1,num_years,1),2),lag=0,
                                lag_type=c(rep("time_lag",num_years),rep("amp_lag",num_years)))
RSS(par=shifts_simulation$lag,df=df,taxa="y_amp",shifts=shifts_simulation,unit_j="year")
lower_lim = -10
upper_lim =  10

df_log <- df %>% mutate(y_amp = log10(abs(y_amp+0.1)),y_phase = log10(y_phase+10.1))
plot(df_log$week,df_log$y_phase)
ggplot() + geom_line(data=df_log,aes(x=week,y=y_amp,color=as.factor(year)),
                     size=1,alpha=0.5)+
  theme_bw() +
  ggtitle("Amplitude change") +
  labs(color="Year",x="Week of Year",y="Amplitude")

RSS_output_amp <- psoptim(par=shifts_simulation$lag,
                          fn=RSS,df=df_log,
                          taxa="y_amp",shifts=shifts_simulation,
                          unit_j="year",
                          lower=rep(lower_lim,num_years),
                          upper=rep(upper_lim,num_years),
                          control=list(maxit=2000))

RSS_output_amp
shifts_simulation$lag = RSS_output$par

plot(df_log$week,df_log$y_phase)
#change in phase
RSS_output_phase <- psoptim(par=shifts_simulation$lag,
                      fn=RSS,df=df_log,
                      taxa="y_phase",shifts=shifts_simulation,
                      unit_j="year",
                      lower=rep(lower_lim,num_years),upper=rep(upper_lim,num_years),
                      control=list(maxit=2000))

shifts_simulation$lag= RSS_output_phase$par
shifts_simulation$lag[1:10]= round(RSS_output_phase$par[1:10])
shifts_simulation %>% ggplot() + geom_tile(aes(x=as.factor(year),y=lag_type,fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Simulation",fill="Lag (Weeks)")

ggsave(filename = paste0(basepath,"/figures/simulation_phase_shift_",Sys.Date(),".png"),
       width=1800,
       height=600,units = "px",dpi=300)

shifts_simulation$lag= RSS_output_amp$par
shifts_simulation$lag[1:10]= round(RSS_output_amp$par[1:10])
shifts_simulation %>% ggplot() + geom_tile(aes(x=as.factor(year),y=lag_type,fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Simulation",fill="Lag (Weeks)")

ggsave(filename = paste0(basepath,"/figures/simulation_amp_shift_",Sys.Date(),".png"),
       width=1800,
       height=600,units = "px",dpi=300)

shifts_simulation %>% filter(lag_type=="time_lag")


shifts_simulation$lag = RSS_output$par
shifts_simulation %>% ggplot() + geom_tile(aes(x=as.factor(year),y=lag_type,fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Simulation",fill="Lag (Weeks)")

#compute correlation
cor_simulation <- cor_year(RSS_optim_year$par,df=df,taxa="y_phase",shifts=shifts_simulation)
ggplot(data=cor_simulation) + geom_point(aes(x=year,y=cor)) + ylim(0,1)
save(RSS_optim_year,cor_simulation,file=paste0(basepath,"/data/RSS_cor_shift_simulation",Sys.Date(),".png"))
