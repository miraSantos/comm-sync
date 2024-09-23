
#find new packages and install them. require all packages in list
list.of.packages <- c("boot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))

files = list.files(paste0(basepath,"results/results_bootstrap/"),full.names=T)
#compute RSS given shifts and relevant units
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#log transform all taxa
log_zero <- function(x){log10(x+0.1)}
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


#creating synthetic data
x = index(df_carbonC_wyear_mean)
period = 53
B = (2*pi)/period
noise = rnorm(length(x),mean = 0, sd= 2)
plot(sin(B*x)+20)
df.sim.noise <- df_carbonC_wyear_mean  %>% select(syear,week,date,year)%>%
                          mutate(baseline = 10*sin(B*x)+20,
                                 test1 = baseline + 
                                   rnorm(length(x),mean = 0, sd= 1),
                                 test2 = baseline +
                                      rnorm(length(x),mean = 0, sd= 2),
                                 test3 = baseline +
                                   rnorm(length(x),mean = 0, sd= 5),
                                 test4 = baseline +
                                   rnorm(length(x),mean = 0, sd= 10),
                                 test5 = baseline +
                                   rnorm(length(x),mean = 0, sd= 20),
                                 test6 = baseline +
                                   rnorm(length(x),mean = 0, sd= 30),
                                 test7 = baseline +
                                   rnorm(length(x),mean = 0, sd= 40))
df.sim.noise$index <-  index(df.sim.noise)


df.sim.noise %>% ggplot() + geom_point(aes(x=index,y=test1)) 
df.sim.noise %>% ggplot() +   geom_point(aes(x=index,y=test4))
df.sim.noise %>% ggplot() +   geom_line(aes(x=index,y=test5))


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

#shifts_year for year level shfits
years <- seq(2006,max(df_carbonC_filled_log$year)-1,1)
shifts_year <- data.frame(year=rep(years,2),lag=0,
                          lag_type=c(rep("time_lag",length(years)),
                                     rep("amp_lag",length(years))))


test_list <- c("baseline","test1","test2","test3","test4","test5","test6","test7")
#retrieve optimal parameters from unconstrained model
time_lag_i = length(shifts_season$year)/2

lower_lim = -4
upper_lim = 4
maxit=1
taxa ="test1"
#number of replicates
p.values.test = data.frame(taxa=character(),p.val=numeric(),c.min=numeric(),c.max=numeric())
for(tt in 1:length(test_list)){
  print(tt)
taxa = test_list[tt]
RSS(par=shifts_season$lag,df=df.sim.noise,
    taxa=taxa,shifts=shifts_season,unit_j="syear")

print("compute optimal shift")
lower_lim = -4
upper_lim = 4
maxit=50
num_replicates = 100
RSS_optim_season <- optim(par=shifts_season$lag,
                            fn=RSS,df=df.sim.noise,
                            taxa=taxa,
                            shifts=shifts_season,
                            unit_j = "syear",
                            fix_t=F,
                            method="L-BFGS-B",
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))


gen_seasonal_mean <- function(par,df,taxa,shifts,unit_j,fix_t=F){
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
  df_shifts <- left_join(df[,c(unit_j,"week","t",taxa)],
                         shifts_wide,by=unit_j) %>%
    #compute new shifts
    mutate(t_shifted = case_when(time_lag>0 ~ pmin(t + time_lag,max(df$t)),
                                 time_lag<0 ~ pmax(t + time_lag,1),
                                 time_lag==0 ~ t)) %>% drop_na()
  #apply shifts to dataframe
  var_shifted = df[df_shifts$t_shifted,taxa]-df_shifts$amp_lag 
  #compute new mean annual cycle
  sub_lag <- function(x){return(x-df_shifts$amp_lag)}
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>%
    group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted) <- c("week","seasonal_mean")
  #create dataframe to compare mean annual cycle
  df_mean_season <- left_join(df_shifts,mean_adjusted,by="week") %>% drop_na()
  return(df_mean_season)
}

df_mean_season <- gen_seasonal_mean(par=RSS_optim_season$par,
                                  df=df.sim.noise,taxa=taxa,
                                  shifts=shifts_season,unit_j="syear",fix_t=F)


shift.statistic <- function(error,taxa="sim",df.shifts,df.sim,unit_j,
                            lower_lim=-4,upper_lim=4,maxit=1){
  #compute error from the original time series by subtracting
  df.sim$sim = df.sim$amp_lag + df.sim$seasonal_mean + error
  #fit simulated data to unconstrained model
  rss_0_lag = psoptim(par=df.shifts$lag,
                      fn=RSS,df=df.sim,
                      taxa=taxa,
                      shifts=df.shifts,
                      unit_j = unit_j,
                      fix_t=T,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  #fit simulated data to unconstrained model
  rss_1_lag = psoptim(par=df.shifts$lag,
                      fn=RSS,df=df.sim,
                      taxa=taxa,
                      shifts=df.shifts, #starting shifts?
                      unit_j = unit_j,
                      fix_t=F,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
  
  #round time lags 
  time_lag_i = length(df.shifts$year)/2
  rss_0_lag[1:time_lag_i] = round(rss_0_lag[1:time_lag_i]) 
  rss_1_lag[1:time_lag_i] = round(rss_1_lag[1:time_lag_i]) 
  #RSS assuming no lag
  RSS_0 = RSS(par = rss_0_lag ,df=df.sim,
              taxa=taxa,
              shifts=df.shifts,unit_j=unit_j,fix_t=T)
  
  #RSS with unconstrained lag
  RSS_1 = RSS(par= rss_1_lag,df=df.sim,
              taxa=taxa,
              shifts=df.shifts,unit_j=unit_j,fix_t=F)
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}

#compute residuals
shift.res.season <- df_mean_season[[taxa]] - df_mean_season$amp_lag -
  df_mean_season$seasonal_mean


#function to generate simulated data
shift.sim <- function(res,n.sim, ran.args) {
  # random generation of replicate series using arima.sim 
  rg1 <- function(n, res) sample(res, n, replace = TRUE)
  df.sim = ran.args$ts
  #return time series with simulated error
  return(df.sim$amp_lag + df.sim$seasonal_mean+rg1)
}

ts.length=length(df.sim.noise$year)
###################################
#bootstrap
###################################
max_iter = maxit
print("bootstrapping residuals")
#perform bootstrap and retrieve values
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
#when fitting to null and unconstrained model
orig.ts.stat = shift.statistic(shift.res.season,df.shifts=shifts_season,df.sim=df_mean_season,
                                unit_j = "syear",taxa = taxa,
                                lower_lim=-4,upper_lim=4,maxit=maxit)

num.greater= length(which(shift.boot.season$t > orig.ts.stat))

shift.boot.season
p.val = num.greater/length(shift.boot.season$t)
hist(shift.boot.season$t)
s.error = sqrt((p.val*(1-p.val))/num_replicates)
c.interval = c(p.val-2*s.error,p.val+2*s.error)
c.interval
p.values.test[tt,] <- c(taxa,p.val,c.interval)

}

p.values.test
simulations_bug_results[["p.values.test"]]
