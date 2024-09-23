basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

files = list.files(paste0(basepath,"/results/results_bootstrap/"),full.names=T)
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso",
                      "rlang")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"data/r_objects/filled/2024-07-26_df_carbonC_filled_wyear_mean.RData"))
source("/home/mira/MIT-WHOI/github_repos/comm-sync/scripts/cyclicity/shift_functions.R")
source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))

#add date time objects
#map months to seasons
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


#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC_filled$year)-1,1)

#expand grid of season per year


#create grid of years to align dataframe and means
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC_filled$year)-1,1)
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = rep(sgrid$Var1,2),year=rep(sgrid$Var2,2),
                            lag = 0, lag_type=c(rep("time_lag",length(sgrid$Var1)),
                                                rep("amp_lag",length(sgrid$Var1)))) %>%
  mutate(syear=paste0(year,"-",season))
shifts_season

#look into files
head(files)


#Create dataframe and loop thru RData Files from the cluster to retrieve shifts from all taxa
super_shift =  data.frame(year=numeric(),lag=numeric(),syear=numeric(),taxa=character(),season=character())
raw_shifts = list()
for(file_i in 1:length(files)){
  load(files[file_i])
  print(file_i)
  seasons <- c("DJF","MAM","JJA","SON")
  years <- seq(2006,max(df_carbonC_filled$year)-1,1)
  sgrid <-expand.grid(seasons,years)
  shifts_season <- data.frame(season = rep(sgrid$Var1,2),year=rep(sgrid$Var2,2),
                              lag = 0, lag_type=c(rep("time_lag",length(sgrid$Var1)),
                                                  rep("amp_lag",length(sgrid$Var1)))) %>%
    mutate(syear=paste0(year,"-",season))
  shifts_season <- shifts_season %>% mutate(lag=RSS_optim_season$par,syear=paste0(year,"-",season))
  shifts_season$taxa = protist_tricho_labelC[file_i]
  shifts_season = left_join(shifts_season,cor_season,by="syear")
  mean_cor = shifts_season %>% group_by(season) %>% summarise(mean_cor = mean(cor,na.rm=T))
  shifts_season<- left_join(shifts_season,mean_cor,by="season",relationship="many-to-many")
  super_shift <- rbind(super_shift,shifts_season)
  raw_shifts[[file_i]] <- RSS_optim_season$par
}

str(super_shift)

###################################
#purpose plot shifts under null and unconstrained model

str(super_shift)
#log transform all taxa
log_zero <- function(x){log10(x+0.1)}
seasons = metseasons[format(df_carbonC_filled$date, "%m")]
df_carbonC_filled_log <- df_carbonC_filled %>%  group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup() %>%  mutate_at(protist_tricho_labelC,log_zero)


#pivot the time lag column wider
super_shift_wide = super_shift %>% mutate(lag = case_when(lag_type=="time_lag"~
                                                            round(lag),
                                                          .default=lag)) %>% pivot_wider(
                                                            names_from="lag_type",values_from="lag")


#select taxa to plot =
kk = which(protist_tricho_labelC=="Ditylum_brightwellii")
which(protist_tricho_labelC=="Corethron_hystrix")
kk = which(protist_tricho_labelC=="Leptocylindrus")
taxa_i = protist_tricho_labelC[kk]

gen_seasonal_mean <- function(par,df,taxa,shifts,unit_j,fix_t=F){
  #create dataframe with years and seasons
  seasons <- c("DJF","MAM","JJA","SON")
  years <- seq(2006,max(df_carbonC_filled$year)-1,1)
  
  #expand grid of season per year
  
  
  #create grid of years to align dataframe and means
  seasons <- c("DJF","MAM","JJA","SON")
  years <- seq(2006,max(df_carbonC_filled$year)-1,1)
  sgrid <-expand.grid(seasons,years)
  shifts_season <- data.frame(season = rep(sgrid$Var1,2),year=rep(sgrid$Var2,2),
                              lag = 0, lag_type=c(rep("time_lag",length(sgrid$Var1)),
                                                  rep("amp_lag",length(sgrid$Var1)))) %>%
    mutate(syear=paste0(year,"-",season))
  shifts_season
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
  df_shifts <- left_join(df[,c(unit_j,"week","date","t",taxa)],
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
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week") %>% drop_na()
  return(df_mean_a)
}

df_mean_season <- gen_seasonal_mean(par=raw_shifts[[kk]],
                                  df=df_carbonC_filled_log,taxa=taxa_i,
                                  shifts=shifts_season,unit_j="syear",fix_t=F)

str(df_mean_season)


df_mean_season %>% ggplot() + geom_point(aes_string(x="date",taxa_i))+
  geom_point(aes(x=date,y=seasonal_mean),color="red")+
  scale_x_date(date_breaks = "2 year",date_labels =format("%Y"))

ggsave(filename=paste0(basepath,"/figures/shift_",taxa_i,"_",Sys.Date(),".png"),
       width=3000,height=800,units="px",dpi=250)

mag_lag <- super_shift_wide %>% mutate(mag_lag = abs(time_lag)) %>% group_by(taxa) %>% summarise(mean_mag_lag = mean(mag_lag,na.rm=T))

mag_lag[order(mag_lag$mean_mag_lag,decreasing=T),]

