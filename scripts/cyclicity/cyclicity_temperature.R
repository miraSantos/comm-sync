
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/2024-06-04_df_carbonC_filled_super_res_paul.RData"))
load(paste0(basepath,"data/r_objects/c_index_df_cor_2024-06-13.RData"))



df_env <- read.csv("/home/mira/MIT-WHOI/Week.2024.05.19-25/mvco_temp_2024.csv")
df_env$date <- as.Date(df_env$timestamp,format ="%Y-%m-%dT%H:%M:%S")
df_env$year <- year(df_env$date)

df_env %>% filter(year>=2006) %>% ggplot() + geom_line(aes(x=date,y=temperature_merge),size=1.2)+
  xlab("Date")+ylab(expression("Temperature ("*degree*"C)"))+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
  theme_bw()

ggsave(filename=paste0(basepath,"/figures/temperature_time_series_",Sys.Date(),".png"),
       width=2000,height=714,units="px",dpi=300)


df_env= df_env %>% filter(year>=2006)
week <- seq(1,53,1)
years <- seq(min(df_env$year),max(df_env$year),1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=year_list)
df_env$year <- year(df_env$date)
df_env$week<-week(df_env$date)

df_env$wyear <- paste0(df_env$week,"-",df_env$year)

week_means <- df_env %>% 
  group_by(week) %>%
  mutate_at(c("temperature_merge"),mean,na.rm=T)

#compute mean temperature for each week of the year
df_env_wyear_mean <-df_env %>%
  select(wyear,temperature_merge,date,year,week) %>%
  group_by(wyear) %>%
  mutate_at(c("temperature_merge"),mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()
df_env_wyear_mean$year <- year(df_env_wyear_mean$date)
df_env_wyear_mean$week <- week(df_env_wyear_mean$date)

#approximate annual cycle with interpolation 
ref_year_interp <- df_env_wyear_mean%>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate(temp_interpolated=na.approx(temperature_merge,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear")) 


#average weekly annual cycle across entire time series
week_climatology = week_means %>% select(temperature_merge,week) %>% distinct()

#create dataframe to store correlations
df_cor <- as.data.frame(matrix(nrow=0,ncol=2))
df_dtw <- as.data.frame(matrix(nrow=0,ncol=2))
names(df_cor) <- c("year","temp_interpolated")
names(df_dtw) <- c("year","temp_interpolated")
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length("temperature_merge")+1))
names(annual_peak) <- c("year","temperature_merge")

#loop through the years and store correlation
for(y in 1:length(years)){
  print(years[y])
  #extract week year means of a specific year
  individual_year <- ref_year_interp %>% ungroup() %>% filter(year == years[y]) %>% select(temp_interpolated)
  #extract diagonal of the correlation matrix
  correlation= diag(cor(week_climatology,individual_year))
  for(i in 1){
  } 
  append_this_cor <- as.data.frame(t(c(year=years[y],correlation)))
  #append to dataframe
  df_cor <- rbind(df_cor,append_this_cor)
  annual_peak <- rbind(annual_peak,c(year=years[y],sapply(individual_year, max, na.rm = TRUE)))
}
names(df_cor) <- c("year","temp_interpolated")

x = seq(1,40,1)
noise = rnorm(length(x),0,sd=0.1)
y=sin(x)
z= sin(x) + 1
dtw(y,z)$normalizedDistance

#take mean of cyclic index
c_index_median = median(df_cor[,"temp_interpolated"])
c_index_sd <- sd(df_cor[,"temp_interpolated"])
# c_index_median_dtw = apply(df_dtw[,protist_tricho_labelC],2,median,na.rm=T)
# consistency.fun <- function(annual_peak){1 - sd(annual_peak - mean(annual_peak))/mean(annual_peak)}
# consistency_index <- apply(annual_peak[,protist_tricho_labelC],2,consistency.fun)

c_index_median
c_index_sd
df_cor

plot(df_cor$temp_interpolated)

individual_year <- ref_year_interp %>% ungroup() %>% filter(year == 2006) %>% select(temp_interpolated)


save(df_env,file=paste0(basepath,"/data/r_objects/df_env_",Sys.Date(),".RData"))
