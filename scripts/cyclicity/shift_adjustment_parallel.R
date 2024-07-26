basepath = "/vortexfs1/scratch/msantos/shift/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","scales","patchwork","pso")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))

args = commandArgs(trailingOnly=TRUE)
jj = as.numeric(args[1])
print(paste("index",jj))

#set lower and upper lim for the shift value
lower_lim=-4
upper_lim=4
maxit = 5000

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


#create version of data at weekly time scale
df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

#create weekly version with sqrt transform
df_carbonC_wyear_mean_sqrt <-df_carbonC_wyear_mean %>%
  mutate_at(protist_tricho_labelC,sqrt)

#set range of years to explore
years = seq(2006,max(df_carbonC_wyear_mean$year),1)

#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC$year)-1,1)



#compute seasonal mean given a list of shifts for each year
seasonal_mean <- function(par_d,df){
  shifts_season$d = round(par_d,digits=0)
  df_shifts <- left_join(df[,c("syear","week")],shifts_season,by="syear") %>%
    mutate(t_shifted = case_when(d>0 ~ pmin(t + d,max(df$x)),
                                 d<0 ~ pmax(t + d,1),
                                 d==0 ~ t))
  var_shifted = df[df_shifts$t_shifted,taxa] 
  mean_adjusted = df[df_shifts$t_shifted,] %>% group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  return(mean_adjusted)
}

#function to compute RSS for shift each season of the year
RSS_season <- function(par_d,df,taxa){
  #set shifts
  shifts_season$d = round(par_d,digits=0)
  #create indexing column
  df$t = index(df)
  #join seasonal shifts to dataframe indices
  df_shifts <- left_join(df[,c("syear","week","t")],shifts_season,by="syear") %>%
    #compute new shifts
    mutate(t_shifted = case_when(d>0 ~ pmin(t + d,max(df$t)),
                                 d<0 ~ pmax(t + d,1),
                                 d==0 ~ t))
  #apply shifts to dataframe
  var_shifted = df[df_shifts$t_shifted,taxa] 
  #compute new mean annual cycle
  mean_adjusted = df[df_shifts$t_shifted,] %>% group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  #create dataframe to compare mean annual cycle
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  #compute residual sum of squares
  shifted_RSS = sum((var_shifted-df_mean_a[taxa])^2,na.rm=T)
  return(shifted_RSS)
}

#function to compute the RSS with shift for each year
RSS_year <- function(par,df,taxa,shifts){
  shifts[["d"]] = round(par,digits=0)
  df$t = index(df)
  df_shifts <- left_join(df[,c("year","week","t")],shifts,by="year") %>%
    #introduce d and set floor and ceiling when adding lag goes out of bounds
    mutate(t_shifted = case_when(d>0 ~ pmin(t + d,max(df$t)),
                                 d<0 ~ pmax(t + d,1),
                                 d==0 ~ t))
  #get abundance at shifted indices
  var_shifted = df[df_shifts$t_shifted,] 
  # get mean weekly abundance at the shifted indices
  mean_adjusted = df[df_shifts$t_shifted,] %>% group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  #join mean weekly to the index of shifts
  df_mean_a <- left_join(df_shifts,mean_adjusted,by="week")
  #compute residual sum of squares (RSS) between abundance and the mean weekly abundance over the shifted indices
  shifted_RSS = sum((var_shifted[taxa]-df_mean_a[taxa])^2,na.rm=T)
  return(shifted_RSS)
}

cor_season <- function(par,df,taxa,shifts){
  df$t = index(df)
  df_shifts <- left_join(df[,c("year","week","t")],shifts,by="year",
                         relationship="many-to-many") %>%
    #introduce d and set floor and ceiling when adding lag goes out of bounds
    mutate(t_shifted = case_when(d>0 ~ pmin(t + d,max(df$t)),
                                 d<0 ~ pmax(t + d,1),
                                 d==0 ~ t))
  #get abundance at shifted indices
  var_shifted = df[df_shifts$t_shifted,] 
  # get mean weekly abundance at the shifted indices
  mean_adjusted = df[df_shifts$t_shifted,] %>% group_by(week) %>% 
    summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted)<-c("week","mean")
  df_mean_a = left_join(var_shifted[c("syear","week",taxa)],mean_adjusted,by="week") %>%
    distinct()
  colnames(df_mean_a) <- c("syear","week","var_shifted","mean")
  cor = df_mean_a %>% group_by(syear) %>% summarise(cor=cor(var_shifted,mean))
  return(cor)
}

cor_year <- function(par,df,taxa,shifts){
  shifts[["d"]] = round(par,digits=0)
  df$t = index(df)
  df_shifts <- left_join(df[,c("year","week","t")],shifts,by="year",
                         relationship="many-to-many") %>%
    #introduce d and set floor and ceiling when adding lag goes out of bounds
    mutate(t_shifted = case_when(d>0 ~ pmin(t + d,max(df$t)),
                                 d<0 ~ pmax(t + d,1),
                                 d==0 ~ t))
  #get abundance at shifted indices
  var_shifted = df[df_shifts$t_shifted,] 
  # get mean weekly abundance at the shifted indices
  mean_adjusted = df[df_shifts$t_shifted,] %>% group_by(week) %>% summarise_at(taxa,mean,na.rm=T)
  colnames(mean_adjusted)<-c("week","mean")
  df_mean_a = left_join(var_shifted[c("year","week",taxa)],mean_adjusted,by="week")
  colnames(df_mean_a) <- c("year","week","var_shifted","mean")
  cor = df_mean_a %>% group_by(year) %>% summarise(cor=cor(var_shifted,mean))
  return(cor)
}


#expand grid of season per year
shifts_season <- expand.grid(seasons,years)
colnames(shifts_season) <- c("season","year")
shifts_season <- shifts_season %>% mutate(d=0,syear=paste0(year,"-",season))

#create grid of years to align dataframe and means
shifts_year <- data.frame(year=years,d=0)



plankton_list_i = protist_tricho_labelC
print(paste("taxa:",plankton_list_i[jj]))
#find optimal set of shifts per season of year that minimize RSS for an individual taxon
RSS_optim_season <- psoptim(par=shifts_season$d,fn=RSS_season,df=df_carbonC_wyear_mean,
                            taxa=plankton_list_i[jj],
                            lower=rep(lower_lim,length(years)),
                            upper=rep(upper_lim,length(years)),
                            control=list(maxit=maxit))

#find optimal set of shifts per year that minimize RSS for an individual taxon
RSS_optim_year <- psoptim(par=shifts_year$d,fn=RSS_year,df=df_carbonC_wyear_mean,
                          taxa=plankton_list_i[jj],shifts=shifts_year,
                          lower=rep(lower_lim,length(years)),
                          upper=rep(upper_lim,length(years)),
                          control=list(maxit=maxit))

#find correlation between each season of year and the mean seasonal cycle
cor_season <- cor_season(par=RSS_optim_season$par,df=df_carbonC_wyear_mean,taxa=plankton_list_i[jj],shifts=shifts_season)

#find correlation between each year and the mean annual cycle
cor_year <- cor_year(par=RSS_optim_season$par,df=df_carbonC_wyear_mean,taxa=plankton_list_i[jj],shifts=shifts_season)

taxa = plankton_list_i[jj]
save(RSS_optim_season,RSS_optim_year,cor_season,cor_year,taxa,
     file=paste0(basepath,"/results/rss_cor_",plankton_list_i[jj],"_",as.character(jj),".RData"))
