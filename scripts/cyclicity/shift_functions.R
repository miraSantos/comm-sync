#find new packages and install them. require all packages in list
list.of.packages <- c("boot","dplyr","tidyr","lubridate","pso","zoo","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


RSS <- function(par,df,taxa,shifts,unit_j,fix_t=F){
  #set shifts
  time_lag_i = length(shifts$year)
  shifts$time_lag = round(par[1:time_lag_i])
  shifts$amp_lag = par[(time_lag_i+ 1):length(par)]
  #round time lags to integers
  #fix time lags to zero
  if(fix_t==T){shifts$time_lag=0}
  #create indexing column
  df$t = index(df)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,taxa,"week","t")],shifts,by=unit_j) %>%
    #compute new shifts
    mutate(t_shifted = case_when(time_lag>0 ~ pmin(t + time_lag,max(df$t)),
                                 time_lag<0 ~ pmax(t + time_lag,1),
                                 time_lag==0 ~ t))
  #apply shifts to dataframe
  var_shifted = df[df_shifts$t_shifted,taxa]-df_shifts$amp_lag 
  #compute new mean annual cycle
  sub_lag <- function(x){return(x-df_shifts$amp_lag)}
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>%
    group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted) = c("week","seasonal_mean")
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  #compute residual sum of squares
  shifted_RSS = sum((var_shifted-(df_mean_a["seasonal_mean"]))^2,na.rm=T)
  return(shifted_RSS)
}


shift.statistic <- function(error){
  taxa_ii="sim"
  df.shifts =  shifts_season
  df.sim = df_mean_a
  unit_j = "syear"
  lower_lim=-4
  upper_lim=4
  maxit = 400
  #compute error from the original time series by subtracting
  df.sim[taxa_ii] = df.sim$amp_lag + df.sim$seasonal_mean + error
  #fit simulated data to constrained model assuming time lag is 0
  rss_0_lag = psoptim(par=rep(0,length(df.shifts$season)*2),
                      fn=RSS,
                      df=df.sim,
                      taxa=taxa_ii,
                      shifts=df.shifts,
                      unit_j = "syear",
                      fix_t=T,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  #fit simulated data to unconstrained model
  rss_1_lag = psoptim(par=rep(0,length(df.shifts$season)*2),
                      fn=RSS,
                      df=df.sim,
                      taxa=taxa_ii,
                      shifts=df.shifts, #starting shifts?
                      unit_j = "syear",
                      fix_t=F,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  
  #round time lags 
  time_lag_i = length(shifts_season$year)
  rss_0_lag[1:time_lag_i] = 0
  rss_1_lag[1:time_lag_i] = round(rss_1_lag[1:time_lag_i])
  #RSS assuming lag is zero
  RSS_0 = RSS(par = rss_0_lag,
              df=df.sim,
              taxa=taxa_ii,
              shifts=df.shifts,
              unit_j=unit_j,
              fix_t=T)
  
  #RSS with unconstrained lag
  RSS_1 = RSS(par= rss_1_lag,
              df=df.sim,
              taxa=taxa_ii,
              shifts=df.shifts,
              unit_j=unit_j,
              fix_t=F)
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}


shift.statistic.orig <- function(error,taxa_ii,
                                 df.shifts =  shifts_season,
                                 df.sim = df_mean_a,
                                 unit_j = "syear",
                                 lower_lim=-4,
                                 upper_lim=4,
                                 maxit = 400){

  #compute error from the original time series by subtracting
  df.sim[taxa_ii] = df.sim$amp_lag + df.sim$seasonal_mean + error
  #fit simulated data to constrained model assuming time lag is 0
  rss_0_lag = psoptim(par=rep(0,length(df.shifts$season)*2),
                      fn=RSS,
                      df=df.sim,
                      taxa=taxa_ii,
                      shifts=df.shifts,
                      unit_j = "syear",
                      fix_t=T,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  #fit simulated data to unconstrained model
  rss_1_lag = psoptim(par=rep(0,length(df.shifts$season)*2),
                      fn=RSS,
                      df=df.sim,
                      taxa=taxa_ii,
                      shifts=df.shifts, #starting shifts?
                      unit_j = "syear",
                      fix_t=F,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  
  #round time lags 
  time_lag_i = length(shifts_season$year)
  rss_0_lag[1:time_lag_i] = 0
  rss_1_lag[1:time_lag_i] = round(rss_1_lag[1:time_lag_i])
  #RSS assuming lag is zero
  RSS_0 = RSS(par = rss_0_lag,
              df=df.sim,
              taxa=taxa_ii,
              shifts=df.shifts,
              unit_j=unit_j,
              fix_t=T)
  
  #RSS with unconstrained lag
  RSS_1 = RSS(par= rss_1_lag,
              df=df.sim,
              taxa=taxa_ii,
              shifts=df.shifts,
              unit_j=unit_j,
              fix_t=F)
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}

RSS.temp <- function(par,df,taxa,shifts,unit_j,fix_t=F){
  #set shifts
  shifts$time_lag = round(par[1] + par[2]*df_season_temp$mean_temp)
  shifts$amp_lag = par[3] + par[4]*df_season_temp$mean_temp
  shifts <- left_join(shifts, df_season_temp,by=c("syear"),
                      relationship="many-to-many")
  #fix time lags to zero
  if(fix_t==T){shifts$time_lag=0}
  #create indexing column
  df$t = index(df)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,taxa,"week","t")],shifts,by=unit_j) %>%
    #compute new shifts
    mutate(t_shifted = case_when(time_lag>0 ~ pmin(t + time_lag,max(df$t)),
                                 time_lag<0 ~ pmax(t + time_lag,1),
                                 time_lag==0 ~ t))
  #apply shifts to dataframe
  var_shifted = df[df_shifts$t_shifted,taxa]-df_shifts$amp_lag 
  #compute new mean annual cycle
  sub_lag <- function(x){return(x-df_shifts$amp_lag)}
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>%
    group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted) = c("week","seasonal_mean")
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  #compute residual sum of squares
  shifted_RSS = sum((var_shifted-(df_mean_a["seasonal_mean"]))^2,na.rm=T)
  return(shifted_RSS)
}

shift.statistic.temp <- function(error){
  taxa_ii="sim"
  df.shifts =  shifts_season
  df.sim = df_mean_season
  unit_j = "syear"
  lower_lim=-4
  upper_lim=4
  maxit = 400
  #compute error from the original time series by subtracting
  df.sim[taxa_ii] = df.sim$amp_lag + df.sim$seasonal_mean + error
  #fit simulated data to constrained model assuming time lag is 0
  rss_0_lag = psoptim(par=rep(0,4),
                      fn=RSS.temp,
                      df=df.sim,
                      taxa=taxa_ii,
                      shifts=df.shifts,
                      unit_j = "syear",
                      fix_t=T,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  #fit simulated data to unconstrained model
  rss_1_lag = psoptim(par=rep(0,4),
                      fn=RSS.temp,
                      df=df.sim,
                      taxa=taxa_ii,
                      shifts=df.shifts, #starting shifts?
                      unit_j = "syear",
                      fix_t=F,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  
  #round time lags 
  #RSS assuming lag is zero
  RSS_0 = RSS.temp(par = rss_0_lag,
              df=df.sim,
              taxa=taxa_ii,
              shifts=df.shifts,
              unit_j=unit_j,
              fix_t=T)
  
  #RSS with unconstrained lag
  RSS_1 = RSS.temp(par= rss_1_lag,
              df=df.sim,
              taxa=taxa_ii,
              shifts=df.shifts,
              unit_j=unit_j,
              fix_t=F)
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}

shift.statistic.orig.temp <- function(error,taxa_ii,
                                 df.shifts =  shifts_season,
                                 df.sim = df_mean_season,
                                 unit_j = "syear",
                                 lower_lim=-4,
                                 upper_lim=4,
                                 maxit = 400){
 
  #compute error from the original time series by subtracting
  df.sim[taxa_ii] = df.sim$amp_lag + df.sim$seasonal_mean + error
  #fit simulated data to constrained model assuming time lag is 0
  rss_0_lag = psoptim(par=rep(0,4),
                      fn=RSS.temp,
                      df=df.sim,
                      taxa=taxa_ii,
                      shifts=df.shifts,
                      unit_j = "syear",
                      fix_t=T,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  #fit simulated data to unconstrained model
  rss_1_lag = psoptim(par=rep(0,4),
                      fn=RSS.temp,
                      df=df.sim,
                      taxa=taxa_ii,
                      shifts=df.shifts, #starting shifts?
                      unit_j = "syear",
                      fix_t=F,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  
  #round time lags 
  #RSS assuming lag is zero
  RSS_0 = RSS.temp(par = rss_0_lag,
                   df=df.sim,
                   taxa=taxa_ii,
                   shifts=df.shifts,
                   unit_j=unit_j,
                   fix_t=T)
  
  #RSS with unconstrained lag
  RSS_1 = RSS.temp(par= rss_1_lag,
                   df=df.sim,
                   taxa=taxa_ii,
                   shifts=df.shifts,
                   unit_j=unit_j,
                   fix_t=F)
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}


gen_seasonal_mean <- function(par,df,taxa,shifts,unit_j,fix_t=F){
  #set shifts
  time_lag_i = length(shifts$year)
  shifts$time_lag = round(par[1:time_lag_i])
  shifts$amp_lag = par[(time_lag_i+ 1):length(par)]
  #round time lags to integers
  #fix time lags to zero
  if(fix_t==T){shifts$time_lag=0}
  #create indexing column
  df$t = index(df)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,taxa,"week","t","season","year")],shifts,
                         by=c(unit_j,"season","year")) %>%
    #compute new shifts
    mutate(t_shifted = case_when(time_lag>0 ~ pmin(t + time_lag,max(df$t)),
                                 time_lag<0 ~ pmax(t + time_lag,1),
                                 time_lag==0 ~ t))
  #apply shifts to dataframe
  var_shifted = df[df_shifts$t_shifted,taxa]-df_shifts$amp_lag 
  #compute new mean annual cycle
  sub_lag <- function(x){return(x-df_shifts$amp_lag)}
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>%
    group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted) = c("week","seasonal_mean")
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by=c("week"))
  return(df_mean_a)
}


#retrieve seasonal mean
gen_seasonal_mean.temp <- function(par,df,df_season_temp,taxa,shifts,unit_j,fix_t=F){
  shifts$time_lag = round(par[1] + par[2]*df_season_temp$mean_temp)
  shifts$amp_lag = par[3] + par[4]*df_season_temp$mean_temp
  shifts <- left_join(shifts, df_season_temp,by=c("syear"),
                      relationship="many-to-many")
  #fix time lags to zero
  if(fix_t==T){shifts$time_lag=0}
  #create indexing column
  df$t = index(df)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,taxa,"week","t")],shifts,by=unit_j) %>%
    #compute new shifts
    mutate(t_shifted = case_when(time_lag>0 ~ pmin(t + time_lag,max(df$t)),
                                 time_lag<0 ~ pmax(t + time_lag,1),
                                 time_lag==0 ~ t))
  #apply shifts to dataframe
  var_shifted = df[df_shifts$t_shifted,taxa]-df_shifts$amp_lag 
  #compute new mean annual cycle
  sub_lag <- function(x){return(x-df_shifts$amp_lag)}
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>%
    group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted) = c("week","seasonal_mean")
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  return(df_mean_a)
}


cor_ts <- function(par,df,taxa,shifts,unit_j){
  #set shifts
  time_lag_i = length(shifts$year)/2
  shifts$time_lag = par[1:time_lag_i]
  shifts$amp_lag = par[time_lag_i+ 1:length(shifts$year)]
  #round time lags to integers
  shifts$time_lag = round(par[1:time_lag_i])
  #create indexing column
  df$t = index(df)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,taxa,"week","t")],shifts,by=unit_j) %>%
    #compute new shifts
    mutate(t_shifted = case_when(time_lag>0 ~ pmin(t + time_lag,max(df$t)),
                                 time_lag<0 ~ pmax(t + time_lag,1),
                                 time_lag==0 ~ t))
  #apply shifts to dataframe
  var_shifted = df[df_shifts$t_shifted,taxa]-df_shifts$amp_lag 
  #compute new mean annual cycle
  sub_lag <- function(x){return(x-df_shifts$amp_lag)}
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>%
    group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted) = c("week","seasonal_mean")
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  #compute residual sum of squares
  cor = df_mean_a %>% group_by(!!sym(unit_j)) %>%
    summarise(cor=cor(!!sym(taxa),seasonal_mean,use="na.or.complete"))
  return(cor)
}



retrieve_lags <- function(output,shifts,fix_t=F){
  shifts$lag = output$par
  time_lag_i = length(shifts$year)/2
  #round time lags to integers
  shifts$lag[1:time_lag_i] = round(output$par[1:time_lag_i])
  #fix time lag 1 to zero
  shifts$lag[1] = 0
  #fix amp lag 1 to zero
  shifts$lag[time_lag_i+1]=0
  #fix time lags to zero
  if(fix_t==T){  shifts$lag[1:time_lag_i]=0}
  amp_lags = shifts %>% filter(lag_type=="amp_lag") %>% select(lag)
  time_lags = shifts %>% filter(lag_type=="time_lag") %>% select(lag)
  shifts_wide = shifts %>% pivot_wider(names_from=lag_type,values_from=lag)
  return(list(shift=shifts_wide,amp_lags = amp_lags, time_lags = time_lags))
}
seasonal_mean <- function(par,df,taxa,shifts,unit_j,fix_t=T){
  #set shifts
  shifts$lag = par 
  time_lag_i = length(shifts$year)/2
  #round time lags to integers
  shifts$lag[1:time_lag_i] = round(par[1:time_lag_i])
  time_lag_i = length(shifts$year)/2
  if(fix_t==T){  shifts$lag[1:time_lag_i]=0}
  shifts_wide= shifts %>% pivot_wider(names_from=lag_type,values_from=lag)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,"week",taxa)],shifts_wide,by=unit_j) %>%
    #compute new shifts
    mutate(week_shifted = (week - time_lag)) %>% mutate(week_shifted = case_when(week_shifted<1~NaN,
                                                                                 week_shifted>53~NaN,
                                                                                 (week_shifted %in% seq(1,53,1))~week_shifted))
  #apply shifts to dataframe
  var_shifted = df[df_shifts$week_shifted,taxa] - df_shifts$amp_lag 
  #compute new mean annual cycle
  sub_lag <- function(x){return(x-df_shifts$amp_lag)}
  mean_adjusted = df_shifts %>% mutate_at(taxa,sub_lag) %>%
    group_by(week_shifted) %>% summarise_at(taxa,mean,na.rm=T)
  return(mean_adjusted)
}

