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

save(df_merged_short,file=paste0(basepath,"/data/r_objects/df_anomaly_merged_short_",Sys.Date(),".RData"))
save(df_merged_long,file=paste0(basepath,"/data/r_objects/df_anomaly_merged_long_",Sys.Date(),".RData"))


#INDIVIDUAL PLOTS


df_merged_long %>% ggplot() +
  geom_point(data=df_merged[df_merged$taxa=="Paralia_sulcata",],aes(x=date,y=log10(anomaly))) +
  geom_point(data=df_merged[df_merged$taxa=="Beam_temperature_corrected",],aes(x=date,y=anomaly),color="red") +
  geom_hline(yintercept=0,color="red") + 
  scale_x_date(date_breaks="1 year",date_labels = format("%Y"))


df_merged_short <- df_merged_long %>%
  pivot_wider(names_from=taxa,values_from=anomaly,id_cols=c("wyear","week","date","year")) %>%
  arrange(date) %>% mutate(week=as.numeric(week))
head(df_merged_short)

df_merged_short %>% mutate_at(label_maybe_include,~log10(.x+1)) %>% ggplot() +
  geom_point(aes(x=week,y=Beam_temperature_corrected))+
  geom_point(aes_string(x="week",y=label_maybe_include[1]),color="green")

taxa = "Acantharia"

df_merged_short %>% ggplot() +
  geom_point(aes_string(x="date",y=taxa))+
  labs(x="Date",y=paste(taxa,"Anomaly"))+
  geom_hline(yintercept = 0,color="red")+
  scale_x_date(date_breaks="1 year",date_labels=format("%Y"))



df_merged_short %>% ggplot() +
  geom_point(aes_string(x="date",y=taxa))+
  labs(x="Date",y=paste(taxa,"Anomaly"))+
  geom_hline(yintercept = 0,color="red")+
  scale_x_date(date_breaks="1 year",date_labels=format("%Y"))

year=2009


#################################################################################
###################################################################################
#create complete week year index (will left join with dataframe later)
week <- seq(1,53,1)

years <- seq(min(df_carbonC$year),max(df_carbonC$year),1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=as.factor(year_list))

#approximate annual cycle with interpolation 
ref_year_interp <- df_carbonC_wyear_mean%>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate_at(protist_tricho_labelC,
            list(~na.approx(object=.,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear"))) 


#average weekly annual cycle across entire time series
week_climatology = week_means_quadroot %>% select(all_of(protist_tricho_labelC))

#create dataframe to store correlations
df_cor <- as.data.frame(matrix(nrow=0,ncol=length(protist_tricho_labelC)+1))
df_dtw <- as.data.frame(matrix(nrow=0,ncol=length(protist_tricho_labelC)+1))
names(df_cor) <- c("year",protist_tricho_labelC)
names(df_dtw) <- c("year",protist_tricho_labelC)
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length(protist_tricho_labelC)+1))
names(annual_peak) <- c("year",protist_tricho_labelC)

#loop through the years and store correlation
for(y in 1:length(years)){
  print(years[y])
  dtw_distance = rep(NA,times=length(protist_tricho_labelC))
  #extract week year means of a specific year
  individual_year <- ref_year_interp %>% ungroup() %>% filter(year == years[y]) %>% select(protist_tricho_labelC)
  #extract diagonal of the correlation matrix
  correlation= diag(cor(week_climatology,individual_year))
  for(i in 1:length(protist_tricho_labelC)){
    dtw_distance[i] <- dtw(individual_year[,i],week_climatology[,i])$normalizedDistance
  } 
  append_this_cor <- as.data.frame(t(c(year=years[y],correlation)))
  append_this_dtw <- as.data.frame(t(c(year=years[y],dtw_distance)))
  #append to dataframe
  df_cor <- rbind(df_cor,append_this_cor)
  df_dtw <- rbind(df_dtw,append_this_dtw)
  annual_peak <- rbind(annual_peak,c(year=years[y],sapply(individual_year, max, na.rm = TRUE)))
}

names(annual_peak) <- c("year",protist_tricho_labelC)
names(df_dtw) <- c("year",protist_tricho_labelC)

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
  geom_hline(yintercept = 0,color="red")+ggtitle(taxa)+
  scale_x_continuous(breaks=seq(1,52,4))
  ggsave(filename=paste0(basepath,"/figures/anomaly/week_colored_year/anomaly_correlation_temperature_",taxa,".png"),
         width=1000,height=800,units="px",dpi=150)
}
quadroot <- function(x){x^(1/4)}

for(taxa in label_maybe_include){
  print(taxa)
  min = -quadroot(abs(min(df_merged_short[,taxa],na.rm=T)))
  max = quadroot(max(df_merged_short[,taxa],na.rm=T))
  for(year_i in seq(2006,2023,1)){
    print(year_i)
    df_merged_short %>% filter(year==year_i)%>%
      mutate_at(label_maybe_include,quadroot) %>% ggplot() +
      geom_hline(yintercept = 0,color="red")+
      geom_point(aes_string(x="week",y=taxa),color='black')+
      geom_point(aes_string(x="week",y="Beam_temperature_corrected"),color='green',
                 shape=4,size=3)+
      labs(x="week",y=paste(taxa,"Anomaly"))+
      scale_x_continuous(breaks=seq(0,52,4))+
      ggtitle(paste(year_i,taxa,"and Temperature"))+
      ylim(min,max)+
      scale_colour_manual(name = 'the colour', 
                          values =c('black'='black','green'='green'), labels = c(taxa,"temp"))
    ggsave(filename=paste0(basepath,"/figures/anomaly/yearly_species/",taxa,"/anomaly_",taxa,"_",year_i,".png"),
           width=800,height=800,units="px",dpi=150)
  }
}
