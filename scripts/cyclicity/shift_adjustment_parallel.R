basepath = "/vortexfs1/scratch/msantos/shift/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","scales","patchwork","
                      
                      pso")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/2024-08-23_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/2024-09-11_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/shift_functions.R"))

args = commandArgs(trailingOnly=TRUE)
jj = as.numeric(args[1])
taxa_i = as.character(args[2])
batch = as.numeric(args[3])
print(paste("index=",jj))
print(paste("taxa=",taxa_i))
print(paste("batch=",batch))
max_iter=500
plankton_list_i = protist_tricho_labelC

#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
num_replicates = 20
print(paste0("maxiter = ",max_iter))
print(paste0("num_replicates = ",num_replicates))



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
         syear=paste0(year,"-",season))

df_carbonC_filled_log <- df_carbonC_filled_wyear_mean %>%
                                  mutate_at(plankton_list_i,log_zero) %>%
                          filter(year>=2006,year<=2022)


#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC_filled_log$year),1)

#expand grid of season per year
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = sgrid$Var1,year=sgrid$Var2,
                            time_lag = 0,amp_lag=0) %>%
  mutate(syear=paste0(year,"-",season))
         
shifts_season

RSS_optim_season <- psoptim(par=c(shifts_season$time_lag,shifts_season$amp_lag),
                            fn=RSS,
                            df=df_carbonC_filled_log,
                            taxa=taxa_i,
                            shifts=shifts_season,
                            unit_j = "syear",
                            fix_t=F,
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=max_iter))


#find correlation between each season of year and the mean seasonal cycle
cor_season <- cor_ts(par=RSS_optim_season$par,df=df_carbonC_filled_log,
                     taxa=taxa_i,shifts=shifts_season,
                     unit_j="syear")




#################################################################################
#bootstrapping residuals
#################################################################################

#function to compute seasonal mean given set of shifts
#retrieve optimal parameters from unconstrained model
time_lag_i = length(shifts_season$year)

#retrieve seasonal mean and parameters from fitting data to unconstrained model
df_mean_a <- gen_seasonal_mean(par=RSS_optim_season$par,
                               df=df_carbonC_filled_log,taxa=taxa_i,
                               shifts=shifts_season,unit_j="syear",fix_t=F)
head(df_mean_a)

lower_lim = -4
upper_lim = 4
#number of replicates

#time series
ts.length = length(df_mean_a$year)

#retrieve ar coefficients
#generate model with ar coefficients
#compute residuals
shift.res.season <- df_mean_a[df_mean_a$t_shifted,][[taxa_i]] - df_mean_a$amp_lag -
  df_mean_a$seasonal_mean


###################################
#bootstrap
###################################
print("Bootstrapping Residuals")
#perform bootstrap and retrieve values
shift.boot.season <- tsboot(tseries = shift.res.season,
                            statistic = shift.statistic,
                            R = num_replicates, #bootstrap replicates required
                            sim = "scramble",
                            n.sim = ts.length, #length of simulated time series
                            orig.t = F)
#max iterations of optimzation #when fitting to null and unconstrained model
str(shift.boot.season)

print("compute original statistic")
#compute statistic of original time series
orig.ts.stat = shift.statistic.orig(error=shift.res.season,
                               taxa = taxa_i,
                               df.sim=df_mean_a,
                               df.shifts=shifts_season,
                               unit_j="syear",
                               lower_lim=-4,upper_lim=4,maxit=max_iter)

################################################################################
print("Saving Results")
###############################################################################

result = list("RSS_optim_season"=RSS_optim_season,
              "cor_season"=cor_season,
              "shift.ts" = shift.boot.season,
              "orig.ts"=orig.ts.stat,
              "taxa_i" = taxa_i,
              "num_rep" = num_replicates)
#save season, year, and correlation files
save(result,
     file=paste0(basepath,"/results/rss_cor",
                "_nr_",num_replicates,
                "_maxit_",max_iter,
                "_batch_",batch,"_",
                 taxa_i,"_",as.character(jj),".RData"))
