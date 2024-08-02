#find new packages and install them. require all packages in list
list.of.packages <- c("boot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

source("/home/mira/MIT-WHOI/github_repos/comm-sync/scripts/cyclicity/shift_functions.R")
load(paste0(basepath,"data/r_objects/filled/2024-07-26_df_carbonC_filled_wyear_mean.RData"))

files = list.files(paste0(basepath,"results_slurm/"),full.names=T)
#compute RSS given shifts and relevant units
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#add seasons and weeks etc to time series
seasons = metseasons[format(df_carbonC_filled$date, "%m")]
df_carbonC_filled <- df_carbonC_filled %>% mutate(doy_numeric = yday(date),
                                    week = week(date),year=year(date),
                                    wyear=paste0(year,"-",week),
                                    season=seasons,
                                    syear=paste0(year,"-",season)) 

lag_length = 136 #length of lag vector (all time and amp lags)

#number of replicates
num_replicates = 100

seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC_filled$year)-1,1)
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = rep(sgrid$Var1,2),year=rep(sgrid$Var2,2),
                            lag = 0, lag_type=c(rep("time_lag",length(sgrid$Var1)),
                                                rep("amp_lag",length(sgrid$Var1)))) %>%
  mutate(syear=paste0(year,"-",season))
shifts_season

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
  df_shifts <- left_join(df[,c(unit_j,"week","t",taxa)],shifts_wide,by=unit_j) %>%
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
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week") %>% drop_na()
   return(df_mean_a)
}

#retrieve optimal parameters from unconstrained model
time_lag_i = length(shifts_season$year)/2
load(files[kk]) #need to get RSS_optim_season

#retrieve seasonal mean and parameters from fitting data to unconstrained model
df_mean_a<- gen_seasonal_mean(par=RSS_optim_season$par,df=df_carbonC_filled,taxa=taxa,
                  shifts=shifts_season,unit_j="syear",fix_t=F)
head(df_mean_a)

maxit=1
lower_lim = -4
upper_lim = 4
taxa = "sim"
taxa = plankton_list_i[kk]
#function to compute statistic
shift.statistic <- function(error,taxa="sim",lower_lim=-4,upper_lim=4,maxit=1){
  #compute error from the original time series by subtracting
  df_mean_a$sim = df_mean_a$amp_lag + df_mean_a$seasonal_mean + error
  #fit simulated data to unconstrained model
  rss_0_lag = psoptim(par=shifts_season$lag,
          fn=RSS,df=df_mean_a,
          taxa=taxa,
          shifts=shifts_season,
          unit_j = "syear",
          fix_t=T,
          lower=rep(lower_lim,length(years)),
          upper=rep(upper_lim,length(years)),
          control=list(maxit=maxit))$par
    #fit simulated data to unconstrained model
    rss_1_lag = psoptim(par=shifts_season$lag,
                      fn=RSS,df=df_mean_a,
                      taxa=taxa,
                      shifts=shifts_season, #starting shifts?
                      unit_j = "syear",
                      fix_t=F,
                      lower=rep(lower_lim,length(years)),
                      upper=rep(upper_lim,length(years)),
                      control=list(maxit=maxit))$par
    
    
  time_lag_i = length(shifts_season$year)/2
  rss_0_lag[1:time_lag_i] = round(rss_0_lag[1:time_lag_i]) 
  #RSS assuming no lag
  RSS_0 = RSS(par = rss_0_lag ,df=df_mean_a,
              taxa=taxa,
              shifts=shifts_season,unit_j="syear",fix_t=T)
  
  #RSS with unconstrained lag
  RSS_1 = RSS(par= rss_1_lag,df=df_mean_a,
              taxa=taxa,
              shifts=shifts_season,unit_j="syear",fix_t=F)
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}

#time series
ts.length = length(df_mean_a$year)

#retrieve ar coefficients
#generate model with ar coefficients

#compute residuals
shift.res <- df_mean_a[[taxa]] - df_mean_a$amp_lag - df_mean_a$seasonal_mean
shift.statistic(shift.res,taxa = taxa,lower_lim=-4,upper_lim=4)

#function to generate simulated data
shift.sim <- function(res,n.sim, ran.args) {
  # random generation of replicate series using arima.sim 
  rg1 <- function(n, res) sample(res, n, replace = TRUE)
  #return time series with simulated error
  return(df_mean_a$amp_lag + df_mean_a$seasonal_mean+rg1)
}


###################################
#bootstrap
###################################
#perform bootstrap and retrieve values
shift.boot <- tsboot(shift.res,
                     shift.statistic,
                     R = num_replicates, #bootstrap replicates required
                     sim = "scramble",
                     n.sim = ts.length, #length of simulated time series
                     orig.t = F,
                     ran.gen = shift.sim,
                     taxa = "sim",
                     lower_lim=-4,
                     upper_lim=4,
                      maxit=500) #max iterations of optimzation when fitting to null and unconstrained model

#compute statistic of original time series
orig.ts.stat = shift.statistic(error=shift.res,taxa = plankton_list_i[kk],
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

