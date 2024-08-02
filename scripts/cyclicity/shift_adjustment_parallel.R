basepath = "/vortexfs1/scratch/msantos/shift/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","scales","patchwork","pso")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/2024-07-26_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/shift_functions.R"))

args = commandArgs(trailingOnly=TRUE)
jj = as.numeric(args[1])
print(paste("index",jj))

#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
maxit = 3000
plankton_list_i = protist_tricho_labelC

#add date time objects
#map months to seasons
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#add seasons and weeks etc to time series
log_zero <- function(x){log10(x+0.1)}
seasons = metseasons[format(df_carbonC_filled$date, "%m")]
df_carbonC_filled_log <- df_carbonC_filled %>%
                                  mutate_at(protist_tricho_labelC,log_zero)


#set range of years to explore
years = seq(2006,max(df_carbonC_filled_log$year),1)

#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC_filled$year)-1,1)

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

#find optimal set of shifts per season of year that minimize RSS for an individual taxon
RSS_optim_season_null <- psoptim(par=shifts_season$lag,
                            fn=RSS,df=df_carbonC_filled_log,
                            taxa=plankton_list_i[jj],
                            shifts=shifts_season,
                            unit_j = "syear",
                            fix_t=T,
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))

RSS_optim_season <- psoptim(par=shifts_season$lag,
                            fn=RSS,df=df_carbonC_filled_log,
                            taxa=plankton_list_i[jj],
                            shifts=shifts_season,
                            unit_j = "syear",
                            fix_t=F,
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))

#find optimal set of shifts per year that minimize RSS for an individual taxon
RSS_optim_year <- psoptim(par=shifts_year$lag,
                          fn=RSS,df=df_carbonC_filled_log,
                          taxa=plankton_list_i[jj],
                          shifts=shifts_year,
                          unit_j = "year",
                          lower=rep(lower_lim,length(years)),
                          upper=rep(upper_lim,length(years)),
                          control=list(maxit=maxit))

#find correlation between each season of year and the mean seasonal cycle
cor_season <- cor_ts(par=RSS_optim_season$par,df=df_carbonC_filled_log,
                     taxa=plankton_list_i[jj],shifts=shifts_season,
                     unit_j="syear")

#find correlation between each year and the mean annual cycle
cor_year <- cor_ts(par=RSS_optim_season$par,df=df_carbonC_filled_log,
                   taxa=plankton_list_i[jj],shifts=shifts_season,
                   unit_j="year")

#save season, year, and correlation files
save(RSS_optim_season_null,RSS_optim_season,RSS_optim_year,cor_season,cor_year,
     file=paste0(basepath,"/results/rss_cor_",plankton_list_i[jj],"_",as.character(jj),".RData"))
