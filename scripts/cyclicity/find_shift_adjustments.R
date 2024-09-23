basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso",
                      "rlang")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-08-23_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/filled/2024-08-23_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))

df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_daily_2023.csv"))
df_env$date <- as.Date(df_env$days,format ="%d-%b-%Y")
df_env$year <- year(df_env$date)
df_env$week <- week(df_env$date)
#add date time objects
#map months to seasons
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#add seasons and weeks etc to time series
#load temperature
seasons = metseasons[format(df_carbonC$date, "%m")]
seasons = metseasons[format(df_env$date, "%m")]

df_env <- df_env %>% mutate(doy_numeric = yday(date),
                            week = week(date),year=year(date),
                            wyear=paste0(year,"-",week),
                            season=seasons,
                            syear=paste0(year,"-",season)) 
df_carbonC <- df_carbonC %>% mutate(doy_numeric = yday(date),
                                    week = week(date),year=year(date),
                                    wyear=paste0(year,"-",week),
                                    season=seasons,
                                    syear=paste0(year,"-",season)) 

log_zero <- function(x){log10(x+0.5*min(x))}

#create version of data at weekly time scale
df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  mutate_at(protist_tricho_labelC,log_zero) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

df_env_weekly <- df_env %>% group_by(wyear) %>%
  mutate_at("Beam_temperature_corrected",mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

#create weekly version with sqrt transform
df_carbonC_wyear_mean_sqrt <-df_carbonC_wyear_mean %>%
  mutate_at(protist_tricho_labelC,sqrt)

#set range of years to explore
years = seq(2006,max(df_carbonC_wyear_mean$year),1)

RSS(par=shifts_season$lag,df=df_env_weekly,
    taxa="Beam_temperature_corrected",shifts=shifts_season,unit_j="syear")

#function to compute RSS for shift each season of the year
RSS(par=shifts_season$lag,df=df_carbonC_wyear_mean,
           taxa=plankton_list_i[1],shifts=shifts_season,unit_j="syear")
#function to compute the RSS with shift for each year

cor_ts(par=shifts_season$lag,df=df_carbonC_wyear_mean,
               taxa=plankton_list_i[1],shifts=shifts_season,unit_j="year")

#expand grid of season per year
#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC$year)-1,1)
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
maxit = 1000
jj = 31
plankton_list_i = protist_tricho_labelC
which(protist_tricho_labelC=="Ditylum_brightwellii")
#find optimal set of shifts per season of year that minimize RSS for an individual taxon

RSS_optim_season <- psoptim(par=shifts_season$lag,
                            fn=RSS,df=df_carbonC_wyear_mean,
                           taxa=plankton_list_i[jj],
                           shifts=shifts_season,
                           unit_j = "syear",
                           fix_t=F,
                           lower=rep(lower_lim,length(years)),
                           upper=rep(upper_lim,length(years)),
                           control=list(maxit=maxit))

RSS_optim_season_temp <- psoptim(par=shifts_season$lag,
                            fn=RSS,df=df_env_weekly,
                            taxa="Beam_temperature_corrected",
                            shifts=shifts_season,
                            unit_j = "syear",
                            fix_t=F,
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))

#find optimal set of shifts per year that minimize RSS for an individual taxon
RSS_optim_year <- psoptim(par=shifts_year$lag,
                          fn=RSS,df=df_carbonC_wyear_mean,
                          taxa=plankton_list_i[jj],
                          shifts=shifts_year,
                          unit_j = "year",
                          fix_t=F,
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
