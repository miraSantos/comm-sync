#find new packages and install them. require all packages in list
list.of.packages <- c("boot","dplyr","tidyr","lubridate","pso","zoo","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


RSS <- function(par,df,taxa,shifts,unit_j,fix_t=F){
  #set shifts
  shifts$lag = par
  time_lag_i = length(shifts$year)/2
  #round time lags to integers
  shifts$lag[1:time_lag_i] = round(par[1:time_lag_i])
  #fix time lags to zero
  if(fix_t==T){shifts$lag[1:time_lag_i]=0}
  #create indexing column
  df$t = index(df)
  shifts_wide = shifts %>% pivot_wider(names_from=lag_type,values_from=lag)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,"week","t")],shifts_wide,by=unit_j) %>%
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
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  #compute residual sum of squares
  shifted_RSS = sum((var_shifted-(df_mean_a[taxa]))^2,na.rm=T)
  return(shifted_RSS)
}


cor_ts <- function(par,df,taxa,shifts,unit_j){
  shifts$lag[1:length(shifts$year)/2] = round(par[1:length(shifts$year)/2])
  shifts$lag[((length(shifts$year)/2)+1):length(shifts$year)] = par[((length(shifts$year)/2)+1):length(shifts$year)]
  
  #create indexing column
  df$t = index(df)
  shifts_wide = shifts %>% pivot_wider(names_from=lag_type,values_from=lag)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,"week","t")],shifts_wide,by=unit_j,
                         relationship="many-to-many") %>%
    #compute new shifts
    mutate(t_shifted = case_when(time_lag>0 ~ pmin(t + time_lag,max(df$t)),
                                 time_lag<0 ~ pmax(t + time_lag,1),
                                 time_lag==0 ~ t))
  #get abundance at shifted indices
  sub_lag <- function(x){return(x-df_shifts$amp_lag)}
  var_shifted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag)
  # get mean weekly abundance at the shifted indices
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>%
    group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted)<-c("week","mean")
  df_mean_a = left_join(var_shifted[c(unit_j,"week",taxa)],mean_adjusted,by="week") %>%
    distinct()
  colnames(df_mean_a) <- c(unit_j,"week","var_shifted","mean")
  cor = df_mean_a %>% group_by(!!sym(unit_j)) %>%
    summarise(cor=cor(var_shifted,mean,use="na.or.complete"))
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
