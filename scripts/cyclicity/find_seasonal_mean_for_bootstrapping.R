#find new packages and install them. require all packages in list
list.of.packages <- c("boot","dplyr","tidyr","lubridate","pso","zoo","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

source("/home/mira/MIT-WHOI/github_repos/comm-sync/scripts/cyclicity/shift_functions.R")
load("/home/mira/MIT-WHOI/github_repos/comm-sync/data/r_objects/filled/2024-07-26_df_carbonC_filled_wyear_mean.RData")
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))

#list output files from results of slurm
files = list.files(paste0(basepath,"results_slurm/"),full.names=T)

load(files[kk])
plankton_list_i = protist_tricho_labelC

#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
maxit = 500
kk = 31
taxa = plankton_list_i[kk]
which(protist_tricho_labelC=="Leptocylindrus")


#compute RSS given shifts and relevant units
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


#find optimal set of shifts per season of year that minimize RSS for an individual taxon

#find optimal parameters under null model
RSS_optim_season_null <- psoptim(par=shifts_season$lag,
                            fn=RSS,df=df_carbonC_filled,
                            taxa=plankton_list_i[kk],
                            shifts=shifts_season,
                            unit_j = "syear",
                            fix_t = T,
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))

shift_opt_null= retrieve_lags(RSS_optim_season,shifts_season,fix_t=T)
head(shift_opt_null)
#find optimal parameters under alternative model (unconstrained)
RSS_optim_season_shift <- psoptim(par=shifts_season$lag,
                            fn=RSS,df=df_carbonC_filled,
                            taxa=plankton_list_i[kk],
                            shifts=shifts_season,
                            unit_j = "syear",
                            fix_t = F,
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))

shift_opt= retrieve_lags(RSS_optim_season,shifts_season,fix_t=F)

#compute smean 
par_null=c(shift_opt_null$shift$time_lag,shift_opt_null$shift$amp_lag)
s_mean_null <- seasonal_mean(par = par_null,
                        df=df_carbonC_filled,
                        taxa=plankton_list_i[kk],
                        shifts=shifts_season,
                        unit_j="syear",
                        fix_t=T)

s_mean  = seasonal_mean(par=RSS_optim_season$par,
                            df=df_carbonC_filled,
                            taxa=plankton_list_i[kk],
                            shifts=shifts_season,
                            unit_j="syear",
                            fix_t=F)

s_mean %>% ggplot() + geom_point(aes(x=week,y=seasonal_mean),color="blue",shape=2,size=3)+
  geom_point(data=s_mean_null,aes(x=week,y=seasonal_mean),color="red")


shift.acf <- compute_acf_resid(df=df_carbonC_filled,
                               taxa=plankton_list_i[kk],
                               s_mean=s_mean,
                               shift_opt=shift_opt)
acf(shift.acf$seasonal_mean)
