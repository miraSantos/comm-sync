basepath = "/vortexfs1/scratch/msantos/shift/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","scales","patchwork","pso")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/2024-08-23_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/2024-08-23_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/shift_functions.R"))

args = commandArgs(trailingOnly=TRUE)
jj = as.numeric(args[1])
taxa_i = as.character(args[2])
batch = as.numeric(args[3])
max_iter=400
print(paste("index=",jj))
print(paste("taxa=",taxa_i))
print(paste("batch=",batch))
print(paste("max_iter=",max_iter))

plankton_list_i = protist_tricho_labelC


#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
plankton_list_i = protist_tricho_labelC
num_replicates = 2

print(paste("maxit =",max_iter))
print(paste("taxa",taxa_i))

#load environmental variable
df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_daily_2023.csv"))
df_env$date <- as.Date(df_env$days,format ="%d-%b-%Y")
df_env$year <- year(df_env$date)
df_env$week <- week(df_env$date)
#add date time objects

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
seasons = metseasons[format(df_carbonC_filled_wyear_mean$date, "%m")]

df_carbonC_filled_wyear_mean <- df_carbonC_filled_wyear_mean %>%
  mutate(doy_numeric = yday(date),
         week = week(date),year=year(date),
         wyear=paste0(year,"-",week),
         season=seasons,
         syear=paste0(year,"-",season))  %>%
  filter((year >=2006 & year <=2022))

str(df_carbonC_filled_wyear_mean)

#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,2022,1)

#expand grid of season per year
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = sgrid$Var1,year=sgrid$Var2,
                            time_lag = 0,amp_lag=0) %>%
  mutate(syear=paste0(year,"-",season))

str(shifts_season)

df_merged = left_join(df_carbonC_filled_wyear_mean,
                      df_env[c("date","Beam_temperature_corrected")],by="date")
str(df_merged)

#compute mean temperature
df_season_temp <- df_merged %>% group_by(syear) %>%
  reframe(mean_temp = mean(Beam_temperature_corrected,na.rm=T),date=first(date)) %>%
  mutate(year = year(date)) %>%
  filter(year>=2006,year<=2022)

str(df_season_temp)

#starting shift parameters 0 only 4
shift_0 = rep(0,4)


RSS.temp(par = shift_0,
         df =df_carbonC_filled_wyear_mean,
         taxa = protist_tricho_labelC[1],
         shifts=shifts_season,
         unit_j="syear",
         fix_t=F)

rss_0_lag = psoptim(par=rep(0,4),
                    fn=RSS.temp,
                    df=df_mean_season,
                    taxa=taxa_ii,
                    shifts=shifts_season,
                    unit_j = "syear",
                    fix_t=T,
                    lower=rep(lower_lim,length(years)),
                    upper=rep(upper_lim,length(years)),
                    control=list(maxit=maxit))$par

#find optimal set of shifts per season of year that minimize RSS for an individual taxon

RSS_optim_season <- psoptim(par=shift_0,
                            fn=RSS.temp,
                            df=df_carbonC_filled_wyear_mean,
                            shifts=shifts_season,
                            unit_j = "syear",
                            taxa=taxa_i,
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=max_iter))

df_mean_season <- gen_seasonal_mean.temp(par=RSS_optim_season$par,
                                         df_season_temp=df_season_temp,
                                    df=df_carbonC_filled_wyear_mean,
                                    taxa=taxa_i,
                                    shifts=shifts_season,unit_j="syear",fix_t=F)

str(df_mean_season)
ts.length=df_mean_season$seasonal_mean

shift.res.season <- df_mean_season[[taxa_i]] - df_mean_season$amp_lag -
  df_mean_season$seasonal_mean
#time series

###################################
#bootstrap
###################################
taxa_ii = taxa_i
RSS.temp(par = rep(0,4),
         df=df_mean_season,
         taxa=taxa_ii,
         shifts=shifts_season,
         unit_j=unit_j,
         fix_t=T)
RSS.temp(par= RSS_optim_season$par,
         df=df_mean_season,
         taxa=taxa_ii,
         shifts=shifts_season,
         unit_j=unit_j,
         fix_t=F)

shift.statistic.temp(shift.res.season)

random_gen <- function(error){return(mean(rnorm(ts.length,mean=0,sd=2)))}

print("Bootstrapping Residuals")
#perform bootstrap and retrieve values
shift.boot.season <- tsboot(tseries = shift.res.season,
                            statistic = shift.statistic.temp,
                            R = num_replicates, #bootstrap replicates required
                            sim = "scramble",
                            n.sim = ts.length, #length of simulated time series
                            orig.t = F)
#max iterations of optimzation #when fitting to null and unconstrained model
str(shift.boot.season)

print("compute original statistic")
#compute statistic of original time series
orig.ts.stat = shift.statistic.orig.temp(error=shift.res.season,
                                    taxa = taxa_i,
                                    df.sim=df_mean_season,
                                    df.shifts=shifts_season,
                                    unit_j="syear",
                                    lower_lim=-4,upper_lim=4,maxit=max_iter)



################################################################################
print("Saving Results")
###############################################################################

result = list("RSS_optim_season"=RSS_optim_season,
              "shift.ts" = shift.boot.season,
              "orig.ts"=orig.ts.stat,
              "taxa_i" = taxa_i,
              "num_rep" = num_replicates)

#save season, year, and correlation files
save(result,
     file=paste0(basepath,"/results/temp/rss_cor+temp",
                 "_nr_",num_replicates,
                 "_maxit_",max_iter,
                 "_batch_",batch,"_",
                 taxa_i,"_",as.character(jj),".RData"))
 