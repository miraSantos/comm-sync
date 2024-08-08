basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso",
                      "rlang")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


source("/home/mira/MIT-WHOI/github_repos/comm-sync/scripts/cyclicity/shift_functions.R")
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"data/r_objects/filled/2024-07-26_df_carbonC_filled_wyear_mean.RData"))
files = list.files(paste0(basepath,"results_slurm/"),full.names=T)
load(paste0(basepath,"data/r_objects/c_index_df_cor_2024-06-27.RData"))

head(c_index)

tol_c_index = c_index$taxa[which(c_index$metric_c_mean > 0.5)]
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


#create grid of years to align dataframe and means
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC$year)-1,1)
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = rep(sgrid$Var1,2),year=rep(sgrid$Var2,2),
                            lag = 0, lag_type=c(rep("time_lag",length(sgrid$Var1)),
                                                rep("amp_lag",length(sgrid$Var1)))) %>%
  mutate(syear=paste0(year,"-",season))
shifts_season



#look into files
head(files)


#Create dataset to load all lags from output of slurm
super_shift =  data.frame(year=numeric(),lag=numeric(),syear=numeric(),taxa=character(),season=character())
for(file_i in 1:length(files)){
load(files[file_i])
  print(file_i)
  seasons <- c("DJF","MAM","JJA","SON")
  years <- seq(2006,max(df_carbonC$year)-1,1)
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
}

str(super_shift)

which()

#set order of season
super_shift <- super_shift %>% mutate(season = factor(season,
                                                      levels=c("DJF","MAM","JJA","SON"))) 

super_shift <- super_shift %>% mutate(lag = case_when(year==2019~NA,((year==2018)&(season=="SON"))~NA,
                                                      (!syear %in% syear_tol) ~ NA,.default=lag))


super_shift %>% filter(season =="DJF",
                       lag_type=="time_lag",
                       taxa %in% tol_c_index) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,-cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)") + ggtitle("Season: DJF")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="SON",
                       lag_type=="time_lag",
                       taxa %in% tol_c_index) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)") + ggtitle("Season: SON")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="MAM",
                       lag_type=="time_lag",
                       taxa %in% tol_c_index) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle("Season: MAM")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(lag_type=="time_lag",
                       (taxa %in% tol_c_index)) %>% ggplot() +
  facet_grid(cols=vars(season))+
  geom_histogram(aes(x=lag))

super_shift %>% filter(season =="JJA",
                       lag_type=="time_lag",
                       taxa %in% tol_c_index,) %>% 
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle("Season: JJA")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="JJA",
                       lag_type=="time_lag",
                       taxa %in% label_maybe_include) %>% group_by(taxa) %>% 
  summarise(avg_mag_lag = mean(abs(lag)))

counts_syear = df_carbonC_wyear_mean %>% group_by(syear) %>% summarise(count=n())
syear_tol = counts_syear$syear[which(counts_syear$count>9)]

super_shift %>% filter(lag_type=="time_lag",
                       (taxa %in% tol_c_index))%>% group_by(season) %>% 
  summarise(avg_mag_lag = mean(abs(lag),na.rm=T))

super_shift %>% filter(lag_type=="time_lag",
                       (taxa %in% tol_c_index))%>% group_by(season) %>% 
  summarise(avg_mag_lag = mean(lag,na.rm=T))

super_shift %>% filter(lag_type=="time_lag",
                       (taxa %in% tol_c_index) & (taxa %in% diatom_labelC))%>% group_by(season) %>% 
  summarise(avg_mag_lag = mean(lag,na.rm=T))

super_shift %>% filter(lag_type=="time_lag",
                       (taxa %in% label_maybe_include) & (taxa %in% dino_label))%>% group_by(season) %>% 
  summarise(avg_mag_lag = mean(abs(lag),na.rm=T))
  
ggsave(filename = paste0(basepath,"/figures/seasonal_shifts/season_shift_","_",Sys.Date(),".png"),
       width = 2400,height= 1200,units="px",dpi=300)

for(kk in 1:length(protist_tricho_labelC)){
  print(kk)
  super_shift %>% filter(taxa==protist_tricho_labelC[kk],lag_type=="time_lag") %>% 
  ggplot() + geom_tile(aes(x=year,y=season,fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD",limits=c(-4,4)) +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle(protist_tricho_labelC[kk])+
    scale_x_continuous(breaks=seq(2006,2023,2))
  

ggsave(filename = paste0(basepath,"/figures/seasonal_shifts/season_shift_",protist_tricho_labelC[kk],"_",Sys.Date(),".png"),
       width = 2400,height= 1200,units="px",dpi=300)
super_shift %>% filter(lag_type == "time_lag") %>% group_by(season) %>% mutate(sd(lag,na.rm=T))

super_shift %>% filter(lag_type == "time_lag") %>% group_by(taxa) %>% ggplot() + 
  geom_point(aes(x=taxa,y=sd(lag,na.rm=T)))

super_shift %>% filter(taxa==protist_tricho_labelC[kk],lag_type=="time_lag") %>% 
  ggplot() + geom_tile(aes(x=year,y=season,fill=cor))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD",limits=c(-1,1)) +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle(protist_tricho_labelC[kk])+
  scale_x_continuous(breaks=seq(2006,2023,2))


ggsave(filename = paste0(basepath,"/figures/seasonal_correlations/season_shift_",protist_tricho_labelC[kk],"_",Sys.Date(),".png"),
       width = 2400,height= 1200,units="px",dpi=300)
}


super_shift %>% filter(season=="SON",lag_type=="time_lag",
                       (taxa %in% diatom_labelC) & (taxa %in% label_maybe_include)) %>% 
  group_by(taxa) %>%
  summarise(avg_mag_lag = mean(abs(lag),na.rm = T)) %>% arrange(desc(avg_mag_lag))


shifts_season <- shifts_season %>% mutate(season = factor(season,
                                         levels=c("SON","JJA","MAM","DJF")),
                                         lag = RSS_optim_season_temp$par,
                                         taxa = "Temperature") 


shifts_season %>% filter(lag_type == "time_lag")%>% ggplot() + 
  geom_tile(aes(x=year,y=season,fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD",limits=c(-4,4))+
  scale_x_continuous(breaks=seq(2006,2023,2))




ggsave(filename = paste0(basepath,"/figures/seasonal_shifts/season_shift_tempeature_",Sys.Date(),".png"),
       width = 2400,height= 1200,units="px",dpi=300)


means_biomass <- colMeans(df_carbonC_wyear_mean[protist_tricho_labelC],na.rm=T)

mb <- stack(means_biomass)
colnames(mb) <- c("mean_conc","taxa")



group = diatom_labelC
super_shift %>% filter(season =="DJF",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,-cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)") + ggtitle("Season: DJF")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="SON",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)") + ggtitle("Season: SON")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="MAM",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle("Season: MAM")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="JJA",
                       lag_type=="time_lag",
                       taxa %in% group) %>% 
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle("Season: JJA")+
  scale_x_continuous(breaks=seq(2006,2023,2))


group = dino_label
super_shift %>% filter(season =="DJF",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,-cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)") + ggtitle("Season: DJF")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="SON",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)") + ggtitle("Season: SON")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="MAM",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle("Season: MAM")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="JJA",
                       lag_type=="time_lag",
                       taxa %in% group) %>% 
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle("Season: JJA")+
  scale_x_continuous(breaks=seq(2006,2023,2))

group = ciliate_label
super_shift %>% filter(season =="DJF",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,-cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)") + ggtitle("Season: DJF")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="SON",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)") + ggtitle("Season: SON")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="MAM",
                       lag_type=="time_lag",
                       taxa %in% group) %>%
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle("Season: MAM")+
  scale_x_continuous(breaks=seq(2006,2023,2))

super_shift %>% filter(season =="JJA",
                       lag_type=="time_lag",
                       taxa %in% group) %>% 
  ggplot() + geom_tile(aes(x=year,y=reorder(taxa,+mean_cor),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD") +
  labs(x="Year",y="Season",fill="Lag (Weeks)")+ ggtitle("Season: JJA")+
  scale_x_continuous(breaks=seq(2006,2023,2))



super_shift %>% filter(lag_type=="time_lag",
                       taxa %in% diatom_labelC) %>% ggplot() +
  facet_grid(cols=vars(season))+
  geom_histogram(aes(x=lag))

super_shift %>% filter(lag_type=="time_lag",
                       taxa %in% dino_label) %>% ggplot() +
  facet_grid(cols=vars(season))+
  geom_histogram(aes(x=lag)) +ylim(0,90)

