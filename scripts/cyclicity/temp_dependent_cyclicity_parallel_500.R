basepath = "/vortexfs1/scratch/msantos/shift/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","scales","patchwork","pso")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/filled/2024-07-26_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/shift_functions.R"))

args = commandArgs(trailingOnly=TRUE)
jj = as.numeric(args[1])
print(paste("index",jj))
print(protist_tricho_labelC[jj])

#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
maxit = 100
plankton_list_i = protist_tricho_labelC
num_replicates = 100
max_iter = 100 #bootstrap

print(paste("maxit =",maxit))
print(paste("taxa",protist_tricho_labelC[jj]))

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

df_carbonC <- df_carbonC %>% mutate(doy_numeric = yday(date),
                                    week = week(date),year=year(date),
                                    wyear=paste0(year,"-",week),
                                    season=seasons,
                                    syear=paste0(year,"-",season))

df_merged = left_join(df_carbonC, df_env[c("date","Beam_temperature_corrected")],by="date")

log_zero <- function(x){log10(x+0.01)}

#create version of data at weekly time scale
df_carbonC_wyear_mean <-df_merged %>% group_by(wyear) %>%  filter(year>2005) %>%
  mutate_at(c(protist_tricho_labelC,"Beam_temperature_corrected"),mean,na.rm=T) %>%
  mutate_at(c(protist_tricho_labelC),log_zero) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

#expand grid of season per year
#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC_wyear_mean$year)-1,1)
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = sgrid$Var1,year=sgrid$Var2)%>%
  mutate(syear=paste0(year,"-",season))

#create grid of years to align dataframe and means
shifts_year <- data.frame(year=rep(years,2))
print("shifts_year")
shifts_year

df_season_temp <- df_merged %>% group_by(syear) %>%
  reframe(mean_temp = mean(Beam_temperature_corrected,na.rm=T),date=first(date))

shift_0 = rep(0,2+length(shifts_season$year))
RSS.temp(par=shift_0,df=df_carbonC_wyear_mean,
         taxa="Beam_temperature_corrected",shifts=shifts_season,unit_j="syear")

#find optimal set of shifts per season of year that minimize RSS for an individual taxon

RSS_optim_season <- psoptim(par=shift_0,
                            fn=RSS.temp,df=df_carbonC_wyear_mean,
                            shifts=shifts_season,
                            unit_j = "syear",
                            taxa=plankton_list_i[jj],
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))


df_mean_season <- gen_seasonal_mean(par=RSS_optim_season$par,
                                    df=df_carbonC_wyear_mean,taxa=plankton_list_i[jj],
                                    shifts=shifts_season,unit_j="syear",fix_t=F)

ts.length=df_mean_season$seasonal_mean

shift.res.season <- df_mean_season[[plankton_list_i[jj]]] - df_mean_season$amp_lag -
  df_mean_season$seasonal_mean

test.shift <- shift.statistic(shift.res.season,df.shifts=shifts_season,df.sim=df_mean_season,
                              unit_j = "syear",taxa = protist_tricho_labelC[jj],
                              lower_lim=-4,upper_lim=4,maxit=1)

print(paste("test shift",test.shift))
#time series

#function to generate simulated data
shift.sim <- function(res,n.sim, ran.args) {
  # random generation of replicate series using arima.sim 
  rg1 <- function(n, res) sample(res, n, replace = TRUE)
  df.sim = ran.args$ts
  #return time series with simulated error
  return(df.sim$amp_lag + df.sim$seasonal_mean+rg1)
}

print("bootstrapping residuals")


print(paste0("num_replicates = ",num_replicates))
print(paste0("max_iter = ",max_iter))


shift.boot.season <- tsboot(tseries = shift.res.season,
                            statistic = shift.statistic,
                            R = num_replicates, #bootstrap replicates required
                            sim = "scramble", #use phase scrambling to generate simulations
                            n.sim = ts.length, #length of simulated time series
                            orig.t = F,
                            ran.gen = shift.sim,
                            taxa = "sim",
                            lower_lim=-4,
                            upper_lim=4,
                            df.shifts=shifts_season,
                            df.sim=df_mean_season,
                            unit_j = "syear",
                            maxit=max_iter,
                            ran.args = list(ts=df.sim)) #max iterations of optimzation 

str(shift.boot.season)

print("compute original statistic")
#compute statistic of original time series
orig.ts.stat = shift.statistic(error=shift.res.season,taxa = plankton_list_i[jj],
                               df.sim=df_mean_season,df.shifts=shifts_season,
                               unit_j="syear",
                               lower_lim=-4,upper_lim=4,maxit=maxit)

#p-value
###proportion of values of the statistic that exceed the original
num.greater= length(which(shift.boot.season$t > orig.ts.stat))

###p-val is fraction of bootstrapped numbers that exceed the original
p.val = num.greater/length(shift.boot.season$t)
hist(shift.boot.season$t)
print(paste("p.val=",p.val))
s.error = sqrt((p.val*(1-p.val))/num_replicates)
c.interval = c(p.val-2*s.error,p.val+2*s.error)
c.interval

print("saving data")
#save season, year, and correlation files
save(RSS_optim_season,
     file=paste0(basepath,"/results/temp_shift_100/rss_cor_",
                 plankton_list_i[jj],"_",as.character(jj),".RData"))

save(shift.boot.season,orig.ts.stat,
     file=paste0(basepath,"results/temp_shift_100/statistic_",
                 plankton_list_i[jj],"_",as.character(jj),".RData"))

print("finished script")
