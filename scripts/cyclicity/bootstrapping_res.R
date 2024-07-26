#find new packages and install them. require all packages in list
list.of.packages <- c("boot","dplyr","tidyr","lubridate","pso","zoo","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

files = list.files(paste0(basepath,"results_slurm/"),full.names=T)
load("/home/mira/MIT-WHOI/github_repos/comm-sync/data/r_objects/filled/2024-06-13_df_carbonC_filled_merged.RData")
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
kk = 1
load(files[kk])
plankton_list_i = protist_tricho_labelC
taxa = plankton_list_i[kk]

#add date time objects
#map months to seasons
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#add seasons and weeks etc to time series
seasons = metseasons[format(df_carbonC$date, "%m")]
df_carbonC <- df_carbonC %>% mutate(doy_numeric = yday(date),
                                    week = week(date),year=year(date),
                                    wyear=paste0(year,"-",week),
                                    season=seasons,
                                    syear=paste0(year,"-",season)) 


#create version of data at weekly time scale
df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

#fill gaps in time series
df_carbonC_filled <- df_carbonC_wyear_mean %>% 
  group_by(date) %>%
  summarize(across(all_of(protist_tricho_labelC),mean)) %>%
  #set to daily frequency
  complete(date = seq.Date(min(df_carbonC$date),max(df_carbonC$date), by="week")) %>%
  #fill out doy_numeric
  mutate(week = week(date)) %>%
  group_by(week)%>%
  #replace nans for living things with yearly mean
  mutate(across(protist_tricho_labelC,~replace_na(.,mean(.,na.rm=T)))) %>%
  select(all_of(c(protist_tricho_labelC,"date","week"))) %>% ungroup()


#add seasons and weeks etc to time series
seasons = metseasons[format(df_carbonC_filled$date, "%m")]
df_carbonC_filled <- df_carbonC_filled %>% mutate(doy_numeric = yday(date),
                                                  week = week(date),year=year(date),
                                                  wyear=paste0(year,"-",week),
                                                  season=seasons,
                                                  syear=paste0(year,"-",season)) 

#compute RSS given shifts and relevant units
RSS <- function(par,df,taxa,shifts,unit_j,fix_t=F){
  #set shifts
  shifts$lag = par
  time_lag_i = length(shifts$year)/2
  #round time lags to integers
  shifts$lag[1:time_lag_i] = round(par[1:time_lag_i])
  #fix time lag 1 to zero
  shifts$lag[1] = 0
  #fix amp lag 1 to zero
  shifts$lag[time_lag_i+1]=0
  #fix time lags to zero
  if(fix_t==T){  shifts$lag[1:time_lag_i]=0}
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
  #fix time lag 1 to zero
  shifts$lag[1] = 0
  #fix amp lag 1 to zero
  shifts$lag[time_lag_i+1]=0
  #fix time lags to zero
  if(fix_t==T){  shifts$lag[1:time_lag_i]=0}
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
  colnames(mean_adjusted) <- c("week","seasonal_mean")
  return(mean_adjusted)
}
compute_acf_resid <- function(df,taxa,s_mean,shift_opt){
  df = df_carbonC_filled %>% select(c(taxa,"week","syear")) %>%
    left_join(s_mean,by="week",relationship="many-to-many") %>% 
    left_join(shift_opt$shift,by="syear") %>% drop_na()
  df$res <- df[[taxa]] - df["seasonal_mean"] - df["amp_lag"]
  return(df$res)
}

#expand grid of season per year
#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC$year)-1,1)

sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = rep(sgrid$Var1,2),year=rep(sgrid$Var2,2),
                            lag = 0, lag_type=c(rep("time_lag",length(sgrid$Var1)),
                                                rep("amp_lag",length(sgrid$Var1)))) %>%
  mutate(syear=paste0(year,"-",season))
str(shifts_season)
#create grid of years to align dataframe and means
shifts_year <- data.frame(year=rep(years,2),lag=0,
                          lag_type=c(rep("time_lag",length(years)),
                                     rep("amp_lag",length(years))))
shifts_year

#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
maxit = 1000
plankton_list_i = protist_tricho_labelC
#find optimal set of shifts per season of year that minimize RSS for an individual taxon

RSS_optim_season <- psoptim(par=shifts_season$lag,
                            fn=RSS,df=df_carbonC_filled,
                            taxa=plankton_list_i[kk],
                            shifts=shifts_season,
                            unit_j = "syear",
                            fix_t = T,
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))

shift_opt= retrieve_lags(RSS_optim_season,shifts_season,fix_t=F)

s_mean <- seasonal_mean(par=RSS_optim_season$par,
                        df=df_carbonC_filled,
                        taxa=plankton_list_i[kk],
                        shifts=shifts_season,
                        unit_j="syear",
                        fix_t=T)
s_mean_null = seasonal_mean(par=rep(0,length(RSS_optim_season$par)),
                            df=df_carbonC_filled,
                            taxa=plankton_list_i[kk],
                            shifts=shifts_season,
                            unit_j="syear",
                            fix_t=T)

s_mean %>% ggplot() + geom_point(aes(x=week,y=seasonal_mean))+
  geom_point(data=s_mean_null,aes(x=week,y=seasonal_mean),color="red")


shift.acf <- compute_acf_resid(df=df_carbonC_filled,
                               taxa=plankton_list_i[kk],
                               s_mean=s_mean,
                               shift_opt=shift_opt)
acf(shift.acf$seasonal_mean)
