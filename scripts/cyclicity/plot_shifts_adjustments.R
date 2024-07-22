### plot lags
shifts_season$par = round(RSS_optim_year$par)
shifts_season %>% ggplot() + geom_tile(aes(x=as.factor(year),y=as.factor(season),fill=par))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")

shiftiness = 1 - (RSS_year(RSS_optim_year$par,df=df,taxa="y_phase",shifts=shifts_simulation)/
                    RSS_year(rep(0,num_years),df=df,taxa="y_phase",shifts=shifts_simulation))
shiftiness