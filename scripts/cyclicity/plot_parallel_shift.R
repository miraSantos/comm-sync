basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

files = list.files(paste0(basepath,"results_slurm/"),full.names=T)
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

#expand grid of season per year

shifts_season <- shifts_season %>% mutate(d=0,syear=paste0(year,"-",season))

#create grid of years to align dataframe and means
shifts_year <- data.frame(year=years,d=0)


super_shift =  data.frame(year=numeric(),d=numeric(),syear=numeric(),taxa=character(),season=character())
for(file_i in 1:length(files)){
load(files[file_i])
  print(file_i)
shifts_season <- expand.grid(seasons,years)
colnames(shifts_season) <- c("season","year")
shifts_season <- shifts_season %>% mutate(d= RSS_optim_season$par,syear=paste0(year,"-",season))
shifts_season$taxa = taxa
shifts_season = left_join(shifts_season,cor_season,by="syear")
mean_cor = shifts_season %>% group_by(season) %>% summarise(mean_cor = mean(cor,na.rm=T))
shifts_season<- left_join(shifts_season,mean_cor,by="season",relationship="many-to-many")
super_shift <- rbind(super_shift,shifts_season)
}

str(super_shift)

super_shift %>% filter(season =="DJF") %>% ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=d))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")

super_shift %>% filter(season =="SON") %>% ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=d))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")

super_shift %>% filter(season =="MAM") %>% ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=d))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")

super_shift %>% filter(season =="JJA") %>% ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=d))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")

ggsave(filename = paste0(basepath,"/figures/seasonal_shifts/season_shift_",taxa,"_",Sys.Date(),".png"),
       width = 2400,height= 1200,units="px",dpi=300)
