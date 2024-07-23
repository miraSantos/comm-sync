
#APPLY TO SIMULATED DATA
df$t = df$x
num_years = max(df$year)
shifts_simulation <- data.frame(year=rep(seq(1,num_years,1),2),lag=0,
                                lag_type=c(rep("time_lag",num_years),rep("amp_lag",num_years)))
RSS(par=c(shifts_simulation$time_lag,shifts_simulation$amp_lag),df=df,
    taxa="y_amp",shifts=shifts_simulation,unit_j="year")
lower_lim = -10
upper_lim =  10

df_log <- df %>% mutate(y_amp = log10(abs(y_amp+0.1)),y_phase = log10(abs(y_amp+0.1)))

ggplot() + geom_line(data=df_log,aes(x=week,y=y_amp,color=as.factor(year)),
                     size=1,alpha=0.5)+
  theme_bw() +
  ggtitle("Amplitude change") +
  labs(color="Year",x="Week of Year",y="Amplitude")

RSS_output <- psoptim(par=shifts_simulation$lag,
                          fn=RSS,df=df_log,
                          taxa="y_amp",shifts=shifts_simulation,
                          unit_j="year",
                          lower=rep(lower_lim,num_years),
                          upper=rep(upper_lim,num_years),
                          control=list(maxit=2000))

RSS_output
plot(RSS_output$par[1:10])
plot(RSS_output$par[11:20])

#change in phase
RSS_output <- psoptim(par=shifts_simulation$lag,
                      fn=RSS,df=df_log,
                      taxa="y_phase",shifts=shifts_simulation,
                      unit_j="year",
                      lower=rep(lower_lim,num_years),upper=rep(upper_lim,num_years),
                      control=list(maxit=5000))

RSS_output
plot(RSS_output$par[1:10]) #time_lag
plot(RSS_output$par[11:20])#amp_lag

ggplot

shifts_simulation$lag= round(RSS_output$par)

shifts_simulation$taxa = "phase_shift"
shifts_simulation %>% ggplot() + geom_tile(aes(x=as.factor(year),y=taxa,fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Simulation",fill="Lag (Weeks)")

ggsave(filename = paste0(basepath,"/figures/simulation_phase_shift_",Sys.Date(),".png"),
       width=1800,
       height=600,units = "px",dpi=300)

shifts_simulation$lag = RSS_output$par
shifts_simulation %>% ggplot() + geom_tile(aes(x=as.factor(year),y=lag_type,fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Simulation",fill="Lag (Weeks)")

#compute correlation
cor_simulation <- cor_year(RSS_optim_year$par,df=df,taxa="y_phase",shifts=shifts_simulation)
ggplot(data=cor_simulation) + geom_point(aes(x=year,y=cor)) + ylim(0,1)
save(RSS_optim_year,cor_simulation,file=paste0(basepath,"/data/RSS_cor_shift_simulation",Sys.Date(),".png"))
