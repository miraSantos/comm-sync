#find new packages and install them. require all packages in list
list.of.packages <- c("boot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

files = list.files(paste0(basepath,"results_slurm/"),full.names=T)
load("/home/mira/MIT-WHOI/github_repos/comm-sync/data/r_objects/filled/2024-06-13_df_carbonC_filled_merged.RData")

kk = 2
load(files[kk])
plankton_list_i = protist_tricho_labelC
taxa = plankton_list_i[kk]

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

#map of month numbers to seasons
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

#compute RSS given shifts and relevant units
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
  mean_adjusted = df[df_shifts$t_shifted,] %>% mutate_at(taxa,sub_lag) %>%
    group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  #compute residual sum of squares
  shifted_RSS = sum((var_shifted-(df_mean_a[taxa]))^2,na.rm=T)
  return(shifted_RSS)
}

lag_length = 136
#applying the statistic RSS_0 - RSS_1
rss_1_lag = RSS_optim_season$par
#set taxa
#compute statistic of original time series
orig.ts.stat = shift.statistic(tsb=df_carbonC_filled[[taxa]])
#number of replicates
num_replicates = 10000

#function to compute statistic
shift.statistic <- function(tsb){
  df = df_carbonC_filled[,c("syear","year","week")]
  df[taxa] = tsb
  rss_0_lag = rep(0,lag_length)
  #RSS assuming no lag
  RSS_0 = RSS(par = rss_0_lag ,df=df,
              taxa=taxa,
              shifts=shifts_season,unit_j="syear")
  #RSS with unconstrained lag
  RSS_1 = RSS(par= rss_1_lag,df=df,
              taxa=taxa,
              shifts=shifts_season,unit_j="syear")
  #statistic
  A = RSS_0 - RSS_1
  return(A)
}

#time series
shift.ts = log10(df_carbonC_filled[[plankton_list_i[kk]]]+0.1)
ts.length = length(df_carbonC_filled$year)

#retrieve ar coefficients
shift.ar<-ar(shift.ts)

#generate model with ar coefficients
shift.model <- list(order = c(shift.ar$order, 0, 0), ar = shift.ar$ar)

#function to generate simulated data
shift.sim <- function(tsb,n.sim, ran.args) {
  # random generation of replicate series using arima.sim 
  rg1 <- function(n, res) sample(tsb, n, replace = TRUE)
  ts.orig <- ran.args$ts
  ts.mod <- ran.args$model
  ts(arima.sim(model = ts.mod, n = n.sim,
                             rand.gen = rg1, res = as.vector(tsb)))
}   

###################################
#bootstrap
###################################
shift.boot <- tsboot(shift.ts,
                     shift.statistic,
                     R = num_replicates, #bootstrap replicates required
                     sim = "model",
                     n.sim = ts.length, #length of simulated time series
                     orig.t = TRUE,
                     ran.gen = shift.sim, 
                     ran.args = list(tsb = shift.ts,
                                     model = shift.model))

#p-value
###proportion of values of the statistic that exceed the original
num.greater= length(which(shift.boot$t > orig.ts.stat))

###p-val is fraction of bootstrapped numbers that exceed the original
p.val = num.greater/length(shift.boot$t)
hist(shift.boot$t)
print(paste("p.val=",p.val))
