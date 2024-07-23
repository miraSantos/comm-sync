basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso",
                      "rlang")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
# load(paste0(basepath,"/data/r_objects/2024-06-04_df_carbonC_filled_super_res_paul.RData"))


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

#create weekly version with sqrt transform
df_carbonC_wyear_mean_sqrt <-df_carbonC_wyear_mean %>%
  mutate_at(protist_tricho_labelC,sqrt)

#set range of years to explore
years = seq(2006,max(df_carbonC_wyear_mean$year),1)

#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC$year)-1,1)



#compute seasonal mean given a list of shifts for each year
seasonal_mean <- function(par_d,df){
  shifts_season$d = round(par_d,digits=0)
  df_shifts <- left_join(df[,c("syear","week")],shifts_season,by="syear") %>%
    mutate(t_shifted = case_when(d>0 ~ pmin(t + d,max(df$x)),
                                 d<0 ~ pmax(t + d,1),
                                 d==0 ~ t))
  var_shifted = df[df_shifts$t_shifted,taxa] 
  mean_adjusted = df[df_shifts$t_shifted,] %>% group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  return(mean_adjusted)
}

#function to compute RSS for shift each season of the year

#function to compute RSS for shift each season of the year
RSS <- function(par,df,taxa,shifts,unit_j){
  #set shifts
  shifts$lag[1:length(shifts$year)/2] = round(par[1:length(shifts$year)/2])
  shifts$lag[((length(shifts$year)/2)+1):length(shifts$year)] = par[((length(shifts$year)/2)+1):length(shifts$year)]
  
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
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>% group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  #compute residual sum of squares
  shifted_RSS = sum((var_shifted-(df_mean_a[taxa]))^2,na.rm=T)
  return(shifted_RSS)
}

RSS(par=shifts_season$lag,df=df_carbonC_wyear_mean,
           taxa=plankton_list_i[1],shifts=shifts_season,unit_j="syear")
#function to compute the RSS with shift for each year

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

cor_ts(par=shifts_season$lag,df=df_carbonC_wyear_mean,
               taxa=plankton_list_i[1],shifts=shifts_season,unit_j="year")

#expand grid of season per year
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = rep(sgrid$Var1,2),year=rep(sgrid$Var2,2),
                            lag = 0, lag_type=c(rep("time_lag",length(sgrid$Var1)),
                                               rep("amp_lag",length(sgrid$Var1)))) %>%
                            mutate(syear=paste0(year,"-",season))
shifts_season
#create grid of years to align dataframe and means
shifts_year <- data.frame(year=rep(years,2),lag=0,
                          lag_type=c(rep("time_lag",length(years)),
                                     rep("amp_lag",length(years))))
shifts_year

#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
maxit = 1
jj = 1
plankton_list_i = protist_tricho_labelC
#find optimal set of shifts per season of year that minimize RSS for an individual taxon

RSS_optim_season <- psoptim(par=shifts_season$lag,
                            fn=RSS,df=df_carbonC_wyear_mean,
                           taxa=plankton_list_i[jj],
                           shifts=shifts_season,
                           unit_j = "syear",
                           lower=rep(lower_lim,length(years)),
                           upper=rep(upper_lim,length(years)),
                           control=list(maxit=maxit))

#find optimal set of shifts per year that minimize RSS for an individual taxon
RSS_optim_year <- psoptim(par=shifts_year$lag,
                          fn=RSS,df=df_carbonC_wyear_mean,
                          taxa=plankton_list_i[jj],
                          shifts=shifts_year,
                          unit_j = "year",
                          lower=rep(lower_lim,length(years)),
                          upper=rep(upper_lim,length(years)),
                          control=list(maxit=maxit))

#find correlation between each season of year and the mean seasonal cycle
cor_season <- cor_ts(par=RSS_optim_season$par,df=df_carbonC_wyear_mean,
                     taxa=plankton_list_i[jj],shifts=shifts_season,
                     unit_j="syear")

#find correlation between each year and the mean annual cycle
cor_year <- cor_ts(par=RSS_optim_season$par,df=df_carbonC_wyear_mean,
                   taxa=plankton_list_i[jj],shifts=shifts_season,
                   unit_j="year")

save(RSS_optim_season,RSS_optim_year,cor_season,cor_year,
     file=paste0(basepath,"/results/rss_cor_",plankton_list_i[jj],"_",as.character(jj),".RData"))
