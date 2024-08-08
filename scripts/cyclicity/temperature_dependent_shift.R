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
shifts_season
#create grid of years to align dataframe and means
shifts_year <- data.frame(year=rep(years,2))
shifts_year


RSS.temp <- function(par,df,taxa,shifts,unit_j,fix_t=F){
  #set shifts
  df_season_temp$time_lag = round(par[1] + par[2]*log10(df_season_temp$mean_temp))
  shifts <- left_join(shifts, df_season_temp,by=c("syear"))
  #fix time lags to zero
  if(fix_t==T){shifts$time_lag=0}
  shifts$amp_lag = par[3:length(par)] 
  #create indexing column
  df$t = index(df)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,"week","t")],shifts,by=unit_j) %>%
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
shift_0 = rep(0,2+length(shifts_season$year))
RSS.temp(par=shift_0,df=df_carbonC_wyear_mean,
    taxa="Beam_temperature_corrected",shifts=shifts_season,unit_j="syear")
#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
maxit = 1000
jj = 31
plankton_list_i = protist_tricho_labelC
which(protist_tricho_labelC=="Ditylum_brightwellii")
#find optimal set of shifts per season of year that minimize RSS for an individual taxon

RSS_optim_season <- psoptim(par=shift_0,
                            fn=RSS.temp,df=df_carbonC_wyear_mean,
                            shifts=shifts_season,
                            unit_j = "syear",
                            taxa=plankton_list_i[jj],
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))

RSS_optim_season

gen_seasonal_mean <- function(par,df,taxa,shifts,unit_j,fix_t=F){
  #set shifts
  df_season_temp$time_lag = round(par[1] + par[2]*log10(df_season_temp$mean_temp))
  shifts <- left_join(shifts, df_season_temp,by=c("syear"))
  #fix time lags to zero
  if(fix_t==T){shifts$time_lag=0}
  shifts$amp_lag = par[3:length(par)] 
  #create indexing column
  df$t = index(df)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c(unit_j,"week","t",taxa)],shifts,by=unit_j) %>%
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
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week") %>% drop_na()
  return(df_mean_a)
}

shift.statistic <- function(error,taxa="sim",df.shifts,df.sim,unit_j,
                            lower_lim=-4,upper_lim=4,maxit=1){
  #compute error from the original time series by subtracting
  df.sim$sim = df.sim$amp_lag + df.sim$seasonal_mean + error
  #fit simulated data to unconstrained model
  rss_0_lag = psoptim(par=shift_0,
                      fn=RSS.temp,df=df.sim,
                      taxa=taxa,
                      shifts=df.shifts,
                      unit_j = unit_j,
                      fix_t=T,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  #fit simulated data to unconstrained model
  rss_1_lag = psoptim(par=shift_0,
                      fn=RSS.temp,df=df.sim,
                      taxa=taxa,
                      shifts=df.shifts, #starting shifts?
                      unit_j = unit_j,
                      fix_t=F,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  
  
  #RSS assuming no lag
  RSS_0 = RSS.temp(par = rss_0_lag ,df=df.sim,
              taxa=taxa,
              shifts=df.shifts,unit_j=unit_j,fix_t=T)
  
  #RSS with unconstrained lag
  RSS_1 = RSS.temp(par= rss_1_lag,df=df.sim,
              taxa=taxa,
              shifts=df.shifts,unit_j=unit_j,fix_t=F)
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}


df_mean_season <- gen_seasonal_mean(par=RSS_optim_season$par,
                                  df=df_carbonC_wyear_mean,taxa=plankton_list_i[jj],
                                  shifts=shifts_season,unit_j="syear",fix_t=F)

shift.res.season <- df_mean_season[[plankton_list_i[jj]]] - df_mean_season$amp_lag -
  df_mean_season$seasonal_mean

shift.statistic(shift.res.season,df.shifts=shifts_season,df.sim=df_mean_season,
                unit_j = "syear",taxa = protist_tricho_labelC[jj],
                lower_lim=-4,upper_lim=4,maxit=100)
#time series
ts.length = length(df_mean_a$year)



#function to generate simulated data
shift.sim <- function(res,n.sim, ran.args) {
  # random generation of replicate series using arima.sim 
  rg1 <- function(n, res) sample(res, n, replace = TRUE)
  df.sim = ran.args$ts
  #return time series with simulated error
  return(df.sim$amp_lag + df.sim$seasonal_mean+rg1)
}
num_replicates = 100
max_iter = 100

shift.boot.season <- tsboot(tseries = shift.res.season,
                            statistic = shift.statistic,
                            R = num_replicates, #bootstrap replicates required
                            sim = "scramble",
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

str(shift.boot.year)
#compute statistic of original time series
orig.ts.stat = shift.statistic(error=shift.res.season,taxa = plankton_list_i[jj],
                               lower_lim=-4,upper_lim=4,maxit=100)

#p-value
###proportion of values of the statistic that exceed the original
num.greater= length(which(shift.boot$t > orig.ts.stat))

###p-val is fraction of bootstrapped numbers that exceed the original
p.val = num.greater/length(shift.boot$t)
hist(shift.boot$t)
print(paste("p.val=",p.val))
s.error = sqrt((p.val*(1-p.val))/num_replicates)
c.interval = c(p.val-2*s.error,p.val+2*s.error)