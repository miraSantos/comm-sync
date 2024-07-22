
#APPLY TO SIMULATED DATA
df$t = df$x
num_years = max(df$year)
shifts_simulation <- data.frame(year=seq(1,num_years,1),d=0)
RSS_year(par=shifts_simulation$d,df=df,taxa="y_phase",shifts=shifts_simulation)
lower_lim = -10
upper_lim =  10

RSS_optim_year <- psoptim(par=shifts_simulation$d,fn=RSS_year,df=df,
                          taxa="y_phase",shifts=shifts_simulation,
                          lower=rep(lower_lim,num_years),upper=rep(upper_lim,num_years),
                          control=list(maxit=1000))

shifts_simulation$par = round(RSS_optim_year$par)
shifts_simulation$taxa = "phase_shift"
shifts_simulation %>% ggplot() + geom_tile(aes(x=as.factor(year),y=taxa,fill=par))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Simulation",fill="Lag (Weeks)")

ggsave(filename = paste0(basepath,"/figures/simulation_phase_shift_",Sys.Date(),".png"),
       width=1800,
       height=600,units = "px",dpi=300)


#compute correlation
cor_simulation <- cor_year(RSS_optim_year$par,df=df,taxa="y_phase",shifts=shifts_simulation)
ggplot(data=cor_simulation) + geom_point(aes(x=year,y=cor)) + ylim(0,1)
save(RSS_optim_year,cor_simulation,file=paste0(basepath,"/data/RSS_cor_shift_simulation",Sys.Date(),".png"))
