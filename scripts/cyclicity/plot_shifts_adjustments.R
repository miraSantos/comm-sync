
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso",
                      "rlang")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-08-23_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/filled/2024-08-23_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))



### plot lags
taxa_i = "Acantharia"

seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,2022,1)
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = rep(sgrid$Var1,2),year=rep(sgrid$Var2,2),
                            lag = 0, lag_type=c(rep("time_lag",length(sgrid$Var1)),
                                                rep("amp_lag",length(sgrid$Var1)))) %>%
  mutate(syear=paste0(year,"-",season))
shifts_season

counts_syear = df_carbonC %>% group_by(syear) %>% summarise(count=n())
counts_syear
syear_tol = counts_syear$syear[which(counts_syear$count>50)]

for(taxa_i in label_maybe_include){
  print(taxa_i)
shifts_season$time_lag = round(super_res[[taxa_i]]$RSS_optim_season$par[1:68])
shifts_season$amp_lag = super_res[[taxa_i]]$RSS_optim_season$par[69:136]



shifts_season %>% 
  mutate(time_lag = case_when(year==2019~NA,
                              ((year==2018)&(season=="SON"))~NA,
                              (!syear %in% syear_tol) ~ NA,.default=time_lag)) %>%
  ggplot() + geom_tile(aes(x=as.factor(year),y=as.factor(season),fill=time_lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="orange", low="darkblue",limits=c(-4,4)) +
  labs(x="Year",y="Season",fill="Lag (Weeks)") 

ggsave(filename=paste0(basepath,"/figures/shift_adjustments/time_lags/shift_adjustments_",taxa_i,"_",Sys.Date(),".png"),
       width=3300,height=1500,units="px",dpi=300)

shifts_season %>% 
  mutate(amp_lag = case_when(year==2019~NA,
                              ((year==2018)&(season=="SON"))~NA,
                              (!syear %in% syear_tol) ~ NA,.default=amp_lag)) %>%
  ggplot() + geom_tile(aes(x=as.factor(year),y=as.factor(season),fill=amp_lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD",limits=c(-1,1)) +
  labs(x="Year",y="Season",fill=expression("Concentration Shift (log"[10]*"("*mu*"g mL"^-1*"))")) 

ggsave(filename=paste0(basepath,"/figures/shift_adjustments/amp_lags/shift_adjustments_",taxa_i,"_",Sys.Date(),".png"),
       width=3300,height=1500,units="px",dpi=300)

}
str(df_carbonC$date)
load("/home/mira/MIT-WHOI/github_repos/comm-sync/data/r_objects/unfilled/2024-08-23_df_carbonC.RData")

metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#add seasons and weeks etc to time series
#load temperature
seasons = metseasons[format(df_carbonC$date, "%m")]

df_carbonC <- df_carbonC %>% mutate(doy_numeric = yday(date),
                                                    week = week(date),year=year(date),
                                                    wyear=paste0(year,"-",week),
                                                    season=seasons,
                                                    syear=paste0(year,"-",season)) 



df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=date,y=Tripos_furca))

df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=date,y=Prorocentrum_dentatum)) 

df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=date,y=Thalassionema_merged)) 

df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=doy_numeric,y=Thalassionema_merged)) 

df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=date,y=Alexandrium_catenella))

df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=date,y=Syracosphaera_pulchra))

df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=date,y=Stenosemella_pacifica))
df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=doy_numeric,y=Stenosemella_pacifica,color=as.factor(year)))

df_carbonC %>% group_by(wyear) %>% mutate(date = first(date)) %>% mutate_at(protist_tricho_labelC,mean,na.rm=T) %>% ungroup() %>%
  ggplot() + geom_point(aes(x=date,y=Warnowia)) + 
  scale_x_date(date_breaks = "2 year",date_labels = format("%Y"))


df_carbonC_filled_wyear_mean %>% ggplot() + geom_point(aes(x=date,y=Prorocentrum_dentatum))
