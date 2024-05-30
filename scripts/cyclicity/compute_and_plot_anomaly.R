#PURPOSE: remove seasonal trend and plot anomalies in abundance

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_26_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_26_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))

load(paste0(basepath,"/data/r_objects/df_env_2024-05-28.RData"))

df_env_merge <-merge(df_carbonC,df_env[,c("Beam_temperature_corrected","date")], by="date") %>%
  mutate(year = year(date),week=week(date),wyear=paste0(week(date),"-",year(date)))

df_merge_wyear_mean <-df_env_merge %>%
  group_by(wyear) %>%
  mutate_at(c(label_maybe_include,"Beam_temperature_corrected"),mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

week_means <- df_env_merge %>% 
  group_by(week) %>%
  summarize_at(c(label_maybe_include,"Beam_temperature_corrected"),mean,na.rm=T)
###################################################################
#Compute cyclic index
#############################################################
#create complete week year index (will left join with dataframe later)
week <- sprintf("%02d",seq(1,53,1))
years <- seq(min(df_env_merge$year),max(df_env_merge$year),1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]

#average weekly annual cycle across entire time series
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=year_list)
week_means_long <- week_means %>% gather(key="taxa",value="mean_conc",-c("week"))  %>% mutate(week = sprintf("%02d",week))
week_means_long_merged <- week_means_long %>% full_join(dfweek,.,by="week",relationship="many-to-many")
df_wyear_long <- df_merge_wyear_mean %>% gather(key="taxa",value="conc",-c("week","year","wyear","pid","date")) %>% 
  select(-c("pid")) %>% mutate(week = sprintf("%02d",week))

head(week_means_long_merged)
head(df_wyear_long)
df_merged_long <-full_join(week_means_long_merged,df_wyear_long,by=c("wyear","taxa","week","year")) %>% 
  drop_na() %>% mutate(anomaly = mean_conc-conc)
head(df_merged_long)


df_merged_long %>% ggplot() +
  geom_point(data=df_merged[df_merged$taxa=="Paralia_sulcata",],aes(x=date,y=log10(anomaly))) +
  geom_point(data=df_merged[df_merged$taxa=="Beam_temperature_corrected",],aes(x=date,y=anomaly),color="red") +
  geom_hline(yintercept=0,color="red") + 
  scale_x_date(date_breaks="1 year",date_labels = format("%Y"))


df_merged_short <- df_merged_long %>%
  pivot_wider(names_from=taxa,values_from=anomaly,id_cols=c("wyear","week","date","year")) %>%
  arrange(date) %>% drop_na()
head(df_merged_short)

df_merged_short %>% mutate_at(label_maybe_include,~log10(.x+1)) %>% ggplot() +
  geom_point(aes(x=week,y=Beam_temperature_corrected))+
  geom_point(aes_string(x="week",y=label_maybe_include[1]),color="green")

taxa = "Acantharia"

cor(df_merged_short$Beam_temperature_corrected,(df_merged_short[label_maybe_include[1]]+1)^(1/4))
df_merged_short %>% ggplot() +
  geom_point(aes_string(x="date",y=taxa))+
  labs(x="Date",y=paste(taxa,"Anomaly"))+
  geom_hline(yintercept = 0,color="red")+
  scale_x_date(date_breaks="1 year",date_labels=format("%Y"))

#################################################################################
#LOOPS
###################################################################################
for(taxa in label_maybe_include){
  print(taxa)
  df_merged_short %>% ggplot() + geom_point(aes_string(x="Beam_temperature_corrected",y=taxa)) +
    geom_smooth(aes_string(x="Beam_temperature_corrected",y=taxa),method="gam")+
    labs(x="Temperature Anomaly",y=paste(taxa,"Anomaly"))
  ggsave(filename=paste0(basepath,"/figures/anomaly/corr_temp/anomaly_correlation_temperature_",taxa,".png"))
}
    

for(taxa in label_maybe_include){
  print(taxa)
df_merged_short %>% mutate(year=as.factor(year)) %>% ggplot() +
  geom_point(aes_string(x="week",y=taxa,color="year"))+
  labs(x="week",y=paste(taxa,"Anomaly"))+
  geom_hline(yintercept = 0,color="red")+ggtitle(taxa)
  ggsave(filename=paste0(basepath,"/figures/anomaly/week_colored_year/anomaly_correlation_temperature_",taxa,".png"))
}
