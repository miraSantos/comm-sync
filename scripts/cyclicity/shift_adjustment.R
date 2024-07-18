basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/2024-06-04_df_carbonC_filled_super_res_paul.RData"))


#add date time objects
#map months to seasons
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#add seasons and weeks etc to time series
seasons = metseasons[format(df_carbonC$date, "%m")]
df_carbonC <- df_carbonC %>% mutate(doy_numeric = yday(date),
                                    week = week(date),year=year(date),
                                    wyear=paste0(year,"-",week),
                                    season=seasons,
                                    syear=paste0(year,"-",season)) 



df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

df_carbonC_wyear_mean$yea

df_carbonC_wyear_mean_sqrt <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup() %>%
  mutate_at(protist_tricho_labelC,sqrt)

#set range of shifts to explore (on the scale of weeks)
#set range of years to explore
years = seq(2006,2022)

plankton_list_i = protist_tricho_labelC
plankton_list_i = diatom_labelC


seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC$year)-1,1)
season_list = rep(seasons,length(years))
year_list <- rep(years,each=4)
shifts <- data.frame(syear=paste0(year_list,"-",season_list),
                     season=season_list,
                     year=year_list,d=0)
shifts

RSS <- function(par_d,df,taxa){
  shifts$d = round(par_d,digits=0)
  df_shifts <- left_join(df[,c("syear","week")],shifts,by="syear") %>%
    mutate(week_shifted = (week + d)%%53) %>%
    mutate(week_shifted = case_when(week_shifted==0 ~ 53,
                                    week_shifted!=0~ week_shifted))
  var_shifted = df[df_shifts$week_shifted,taxa] 
  mean_adjusted = df[df_shifts$week_shifted,] %>% group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
    
  shifted_RSS = sum((var_shifted[taxa]-df_mean_a[taxa])^2,na.rm=T)
  return(shifted_RSS)
}

RSS(shifts$d,df=df_carbonC_wyear_mean,taxa=protist_tricho_labelC)
lower_lim=-4
upper_lim=4
RSS_optim=list()
for (ii in 1:length(protist_tricho_labelC)){
  print(paste(ii,"of",length(protist_tricho_labelC)))
#find minimum RSS with min
RSS_optim[[ii]] <- psoptim(par=shifts$d,fn=RSS,df=df_carbonC_wyear_mean,
                     taxa=protist_tricho_labelC[1],
                     lower=rep(lower_lim,length(years)),upper=rep(upper_lim,length(years)),control=list(maxit=300))
}
