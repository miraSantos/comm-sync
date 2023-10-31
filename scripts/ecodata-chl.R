list.of.packages <- c("ggplot2","remotes","knitr","rmarkdown",
                      "readxl","lubridate","dplyr","sf")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)
#see technical documentation here: https://noaa-edab.github.io/tech-doc/

######################
#CHLOROPHYLL
#####################

extract_chl_pp <- function(Variable){
  
  df <- ecodata::chl_pp %>% 
    dplyr::filter(stringr::str_detect(Var,Variable)) %>% 
    tidyr::separate(.,Time, into = c("Cat", "Time2"), sep = "_") %>% 
    tidyr::separate(.,Time2, into = c("year", "week"), sep = 4) %>%
    dplyr::mutate(Year = as.numeric(year),week = as.numeric(week),
                  date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u"))
  
  metseasons <- c(
    "01" = "Winter", "02" = "Winter",
    "03" = "Spring", "04" = "Spring", "05" = "Spring",
    "06" = "Summer", "07" = "Summer", "08" = "Summer",
    "09" = "Fall", "10" = "Fall", "11" = "Fall",
    "12" = "Winter")
  
  seasons = metseasons[format(df$date, "%m")]
  df$season = seasons
  
  regime_1_end = 2012
  regime_2_end = 2018
  regime_1_index = (which(df$year < regime_1_end))
  regime_2_index = (which((df$year >= regime_1_end)&(df$year < regime_2_end)))
  regime_3_index = (which(df$year >= regime_2_end))
  
  df$regime = NA
  
  df$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
  df$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
  df$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")
  
  df$doy_numeric = yday(df$date)
  
  return(df)
}

df_chl = extract_chl_pp(Variable = "WEEKLY_CHLOR_A_MEDIAN")
write.csv(df_chl,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/weekly_chl_ecomon.csv")

df_pp = extract_chl_pp(Variable = "WEEKLY_PPD_MEDIAN")
write.csv(df_pp,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/weekly_ppd_ecomon.csv")

plot_season_year <- function(dataset,season_opt,epu,basepath,ymin=0,ymax){
  variable = dataset$Var[1]
  dataset %>% filter(season ==season_opt,Year>2005,EPU==epu) %>% ggplot(aes(x=as.factor(week)))+
    geom_point(aes(y=Value))+  facet_grid(cols=vars(year))+
    scale_x_discrete(breaks=seq(1,52,5))+
    ylab(paste0(variable," (",dataset$Units[1],")"))+
    ggtitle(paste0(season_opt," ",variable," in the ",epu))+
    xlab("Week of Year")+
    ylim(0,ymax)
  ggsave(paste0(basepath,variable,"_",season_opt,"_",epu,".png"),width = 2000,height = 500,units = "px",dpi = 150)
}

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/environmental/"
plot_season_year(df_chl,"Summer","MAB",basepath,ymax=1.5)
plot_season_year(df_chl,"Summer","GOM",basepath,ymax = 1.5)
plot_season_year(df_chl,"Summer","GB",basepath,ymax = 1.5)


plot_season_year(df_pp,"Summer","GOM",basepath,ymax = 2.5)
plot_season_year(df_pp,"Summer","MAB",basepath,ymax = 2.5)
plot_season_year(df_pp,"Summer","GB",basepath,ymax = 2.5)


plot_season_year(df_chl,"Spring","MAB",basepath,ymax=1.5)
plot_season_year(df_chl,"Spring","GOM",basepath,ymax = 1.5)
plot_season_year(df_chl,"Spring","GB",basepath,ymax = 1.5)

plot_season_year(df_pp,"Spring","GOM",basepath,ymax = 2)
plot_season_year(df_pp,"Spring","MAB",basepath,ymax = 2.5)
plot_season_year(df_pp,"Spring","GB",basepath,ymax = 2.5)

plot_season_year(df_chl,"Fall","MAB",basepath,ymax=2.5)
plot_season_year(df_chl,"Fall","GOM",basepath,ymax = 2.5)
plot_season_year(df_chl,"Fall","GB",basepath,ymax = 2.5)


plot_season_year(df_pp,"Fall","GOM",basepath,ymax = 2.5)
plot_season_year(df_pp,"Fall","MAB",basepath,ymax = 2.5)
plot_season_year(df_pp,"Fall","GB",basepath,ymax = 2.5)


plot_season_anomaly <- function(dataset,season_opt,epu){
variable = dataset$Var[1]
dataset = dataset %>% filter(Year>2005,EPU==epu) %>% group_by(doy_numeric) %>% mutate(doy_mean = mean(Value)) %>% ungroup()
dataset %>% filter(season ==season_opt,Year>2005,EPU==epu) %>% ggplot(aes(x=as.factor(week)))+
  geom_point(aes(y=Value-doy_mean))+  facet_grid(cols=vars(year))+
  geom_hline(aes(yintercept=0),color="red")+
  scale_x_discrete(breaks=seq(1,52,5))+
  ylab(paste0("PP Anomaly"," (",dataset$Units[1],")"))+
  ggtitle(paste0(season_opt," ","PP Anomaly"," in the ",epu))+
  xlab("Week of Year")
ggsave(paste0(basepath,"weekly_ppd_anomaly","_",season_opt,"_",epu,".png"),width = 2000,height = 500,units = "px",dpi = 150)
}

plot_season_anomaly(df_pp,"Spring","GOM")
plot_season_anomaly(df_pp,"Spring","MAB")
plot_season_anomaly(df_pp,"Spring","GB")


plot_season_anomaly(df_pp,"Summer","GOM")
plot_season_anomaly(df_pp,"Summer","MAB")
plot_season_anomaly(df_pp,"Summer","GB")

plot_season_anomaly(df_pp,"Fall","GOM")
plot_season_anomaly(df_pp,"Fall","MAB")
plot_season_anomaly(df_pp,"Fall","GB")


###############################################################
#RATIO ANOMALY
###############################################################
df_chl_ratio = extract_chl_pp(Variable = "WEEKLY_CHLOR_A_RATIO_ANOMALY")
df_pp_ratio = extract_chl_pp(Variable = "WEEKLY_PPD_RATIO_ANOMALY")

plot_season_year(df_chl_ratio,"Summer","MAB",basepath,ymin=-2,ymax=1.5)
plot_season_year(df_chl_ratio,"Summer","GOM",basepath,ymin=-2,ymax = 1.5)
plot_season_year(df_chl_ratio,"Summer","GB",basepath,ymin=-2,ymax = 1.5)


plot_season_year(df_pp_ratio,"Spring","GOM",basepath,ymin=-2,ymax = 2)
plot_season_year(df_pp_ratio,"Spring","MAB",basepath,ymin=-2,ymax = 2)
plot_season_year(df_pp_ratio,"Spring","GB",basepath,ymin=-2,ymax = 2)


plot_season_year(df_pp_ratio,"Summer","GOM",basepath,ymin=-2,ymax = 2)
plot_season_year(df_pp_ratio,"Summer","MAB",basepath,ymin=-2,ymax = 2)
plot_season_year(df_pp_ratio,"Summer","GB",basepath,ymin=-2,ymax = 2)

plot_season_year(df_pp_ratio,"Fall","GOM",basepath,ymin=-2,ymax = 2)
plot_season_year(df_pp_ratio,"Fall","MAB",basepath,ymin=-2,ymax = 2)
plot_season_year(df_pp_ratio,"Fall","GB",basepath,ymin=-2,ymax = 2)




head(df_chl_ratio)

###################
#REGIME PLOTS
#####################
plot_week_regime <- function(dataset,epu,basepath){
  var = dataset$Var[1]
  dataset %>% filter(EPU == epu) %>% ggplot() + geom_boxplot(aes(x = as.factor(week), y = Value))+
    scale_x_discrete(breaks=seq(1,52,5))+
    facet_grid(cols=vars(regime))+
    ylab(paste0(var," (",dataset$Units[1],")"))+
    ggtitle(paste(var,"in the",epu))
  
  ggsave(paste0(basepath,var,"_regime","_",epu,".png"))
}

plot_week_regime(dataset=df_chl,"GOM",basepath)
plot_week_regime(dataset=df_chl,"GB",basepath)
plot_week_regime(dataset=df_chl,"MAB",basepath)

plot_week_regime(dataset=df_pp,"GOM",basepath)
plot_week_regime(dataset=df_pp,"GB",basepath)
plot_week_regime(dataset=df_pp,"MAB",basepath)

plot_week_regime(dataset=df_chl_ratio,"GOM",basepath)
plot_week_regime(dataset=df_chl_ratio,"GB",basepath)
plot_week_regime(dataset=df_chl_ratio,"MAB",basepath)

plot_week_regime(dataset=df_pp_ratio,"GOM")
plot_week_regime(dataset=df_pp_ratio,"GB")
plot_week_regime(dataset=df_pp_ratio,"MAB")


#####################################################
# BOTTOM TEMPERATURE
####################################################

extract_bottom_temp <- function(Variable){
  
  df <- ecodata::bottom_temp %>% 
    dplyr::filter(stringr::str_detect(Var,Variable)) %>% 
    tidyr::separate(.,Time, into = c("Cat", "Time2"), sep = "_") %>% 
    tidyr::separate(.,Time2, into = c("year", "week"), sep = 4) %>%
    dplyr::mutate(Year = as.numeric(year),week = as.numeric(week),
                  date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u"))
  
  metseasons <- c(
    "01" = "Winter", "02" = "Winter",
    "03" = "Spring", "04" = "Spring", "05" = "Spring",
    "06" = "Summer", "07" = "Summer", "08" = "Summer",
    "09" = "Fall", "10" = "Fall", "11" = "Fall",
    "12" = "Winter")
  
  seasons = metseasons[format(df$date, "%m")]
  df$season = seasons
  
  regime_1_end = 2012
  regime_2_end = 2018
  regime_1_index = (which(df$year < regime_1_end))
  regime_2_index = (which((df$year >= regime_1_end)&(df$year < regime_2_end)))
  regime_3_index = (which(df$year >= regime_2_end))
  
  df$regime = NA
  
  df$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
  df$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
  df$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")
  
  return(df)
}

str(ecodata::bottom_temp)

df_bottom_temp = extract_bottom_temp(Variable = "bottom temp anomaly in situ")

head(df_bottom_temp)

plot_bottom_temp <- function(epu,var){
ecodata::bottom_temp %>%
  filter(EPU==epu,Var==var,Time >=2005) %>%
  ggplot() + geom_point(aes(x=Time,y=Value))+
  scale_x_continuous(breaks=seq(2005,2021,1))+
  ylab("Bottom Temperature Anomaly")+
  ggtitle(paste("Bottom Temperature Anomaly in the",epu))
ggsave(paste0(basepath,"bottom_temp","_",epu,".png"))
}

plot_bottom_temp("MAB","bottom temp anomaly in situ")
plot_bottom_temp("GOM","bottom temp anomaly in situ")
plot_bottom_temp("GB","bottom temp anomaly in situ")

