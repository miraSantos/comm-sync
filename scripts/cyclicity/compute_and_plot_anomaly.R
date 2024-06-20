#PURPOSE: remove seasonal trend and plot anomalies in abundance

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","rlang","dtw","scales","patchwork",
                      "moments")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/c_index_merged_df_cor_2024-06-05.RData"))

load(paste0(basepath,"/data/r_objects/df_env_2024-05-28.RData"))

df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_daily_2023.csv"))
head(df_env)
df_env$date <- as.Date(df_env$days,format ="%d-%b-%Y")

df_env_merge <-merge(df_carbonC,df_env[,c("Beam_temperature_corrected","AvgSolar","date")], by="date") %>%
  mutate(year = year(date),week=week(date),wyear=paste0(week(date),"-",year(date)))

df_merge_wyear_mean <-df_env_merge %>%
  group_by(wyear) %>%
  mutate_at(c(label_maybe_include,"Beam_temperature_corrected","AvgSolar"),mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()


week_medians <- df_env_merge %>% 
  group_by(week) %>%
  summarize_at(c(label_maybe_include,"Beam_temperature_corrected","AvgSolar"),median,na.rm=T)

###################################################################
#Compute anomaly
#############################################################
#create complete week year index (will left join with dataframe later)
week <- sprintf("%02d",seq(1,53,1))
years <- seq(min(df_env_merge$year),max(df_env_merge$year),1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]

#average weekly annual cycle across entire time series
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=year_list)
week_medians_long <- week_medians %>% gather(key="taxa",value="mean_conc",-c("week"))  %>% mutate(week = sprintf("%02d",week))
week_medians_long_merged <- week_medians_long %>% full_join(dfweek,.,by="week",relationship="many-to-many")
df_wyear_long <- df_merge_wyear_mean %>% gather(key="taxa",value="conc",-c("week","year","wyear","pid","date")) %>% 
  select(-c("pid")) %>% mutate(week = sprintf("%02d",week))

head(week_medians_long_merged)
head(df_wyear_long)
df_merged_long <-full_join(week_medians_long_merged,df_wyear_long,by=c("wyear","taxa","week","year")) %>% 
  drop_na() %>% mutate(anomaly = conc-mean_conc)
head(df_merged_long)

#create short version
df_merged_short_anomaly <- df_merged_long %>%
  pivot_wider(names_from=taxa,values_from=anomaly,id_cols=c("wyear","week","date","year")) %>%
  arrange(date) %>% mutate(week=as.numeric(week))
head(df_merged_short_anomaly)


###########################################################################3
#analyze anomaly stats
#################################################################################


df_anomaly_stats <- df_merged_long %>% group_by(taxa) %>%
  summarise(kurtosis=kurtosis(anomaly,na.rm=T),skewness=skewness(anomaly,na.rm=T),
            var=var(anomaly,na.rm=T))
head(df_anomaly_stats)

func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton","Metazoan","Cyanobacteria","Picoeukaryotes")
func_group_labels <- list(diatom_labelC,dino_label,ciliate_label,nfg_label,metazoan_label,c("Synechococcus","Trichodesmium"),c("Pico_eukaryotes"))
#create column with functional group 
for(func_group in 1:length(func_group_list)){
  reference=func_group_labels[[func_group]]
  df_anomaly_stats[df_anomaly_stats$taxa%in%reference,"func_group"] = func_group_list[func_group]
}

#plot skew for all taxa
my_colors <- RColorBrewer::brewer.pal(7, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(df_anomaly_stats[order(df_anomaly_stats$skewness,df_anomaly_stats$func_group),],map,by="func_group",relationship = "many-to-many")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group

df_anomaly_stats %>% filter(taxa %in% label_maybe_include) %>% 
  ggplot() + geom_bar(aes(x=reorder(taxa,+skewness),y=skewness,fill=func_group),stat="identity")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  coord_flip()+
  labs(x="Taxa",y="Skewness")

#plot kurtosis for all taxa
my_colors <- RColorBrewer::brewer.pal(7, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(df_anomaly_stats[order(df_anomaly_stats$kurtosis,df_anomaly_stats$func_group),],map,by="func_group",relationship = "many-to-many")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group

df_anomaly_stats %>% filter(taxa %in% label_maybe_include) %>% ggplot() +
  geom_bar(aes(x=reorder(taxa,+kurtosis),y=sqrt(kurtosis),
               fill=func_group),stat="identity")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  coord_flip()+
  geom_hline(aes(yintercept=3),color="red")+
  labs(x="Taxa",y=expression("Kurtosis"^(1/2)))

save(df_merged_short_anomaly,file=paste0(basepath,"/data/r_objects/df_anomaly_merged_short_",Sys.Date(),".RData"))
save(df_merged_long,file=paste0(basepath,"/data/r_objects/df_anomaly_merged_long_",Sys.Date(),".RData"))
save(df_anomaly_stats,file=paste0(basepath,"/data/r_objects/df_anomaly_stats_",Sys.Date(),".RData"))
#####################################################################
#Plot anomaly
#####################################################################

kurtosis(df_merged_short_anomaly$Beam_temperature_corrected,na.rm=T)
skewness(df_merged_short_anomaly$Beam_temperature_corrected,na.rm=T)

kurtosis(df_merged_short_anomaly$AvgSolar,na.rm=T)
skewness(df_merged_short_anomaly$AvgSolar,na.rm=T)



#plot histogram of temperature anomaly
df_merged_short_anomaly  %>%
  ggplot() + geom_histogram(aes_string(x="Beam_temperature_corrected"),bins=100)+
  geom_vline(aes(xintercept=0),color="red")+
  xlab(expression("Temperature anomaly ("*degree*"C)"))+
  ylab("Count")+theme_bw()


ggsave(filename=paste0(basepath,"/figures/anomaly/histogram_temperature_",Sys.Date(),".png"),width=800,height=600,units="px",dpi=150)


for(taxa_i in label_maybe_include){
  print(taxa_i)
  df_merged_short_anomaly  %>%
    ggplot() + geom_histogram(aes_string(x=taxa_i),bins=100)+
    geom_vline(aes(xintercept=0),color="red")+
    xlab(paste(taxa_i,"anomaly"))+
    ylab("Count")
  
  ggsave(filename =paste0(basepath,"/figures/anomaly/histograms/",taxa_i,"_anomaly_histogram_",Sys.Date(),".png"),
         width=800,height=600,units="px",dpi=150)

}

ggsave(filename =paste0(basepath,"/figures/anomaly/histograms/temperature_anomaly_histogram_",Sys.Date(),".png"),
       width=800,height=600,units="px",dpi=150)


  #plot week by taxa and temperature
df_merged_short_anomaly %>% mutate_at(label_maybe_include,~log10(.x+1)) %>% ggplot() +
  geom_point(aes(x=week,y=Beam_temperature_corrected))+
  geom_point(aes_string(x="week",y=label_maybe_include[1]),color="green")

#plot individual anomaly 
taxa = "Acantharia"
df_merged_short_anomaly %>% ggplot() +
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

df_merged_short_anomaly <- df_merged_short_anomaly %>% mutate(month=month(date)) %>%
  mutate(season = if_else(month %in% c(6,7,8),"JJA",if_else(month %in% c(12,1,2),"DJF",
                                                            if_else(month %in% c(3,4,5),"MAM",
                                                                    if_else(month %in% c(9,10,11),"SON",
                                                                            "Other")))))

################################################################################
#Plotting just temperature anomaly
################################################################################
df_merged_short_anomaly %>% mutate(Sign = if_else(Beam_temperature_corrected >= 0, "Positive", "Negative")) %>%
ggplot(aes_string(x="date",y="Beam_temperature_corrected")) +
  geom_line(show.legend = FALSE)+
  # geom_area(aes(fill=Sign), alpha = 0.8,position="identity",show.legend = FALSE)+
  scale_fill_manual(values=c("blue","red"))+
  scale_color_manual(values=c("blue","red"))+
  labs(x="Date",y=expression("Temperature Anomaly ("*degree*"C)"))+
  geom_hline(yintercept = 0,color="black")+
  scale_x_date(date_breaks="1 year",date_labels=format("%Y"))

df_merged_short_anomaly %>% 
  mutate(Sign = if_else(Beam_temperature_corrected >= 0, "Positive", "Negative")) %>%
ggplot(aes(date,Beam_temperature_corrected, ymin = 0,ymax = Beam_temperature_corrected,
                               color = Sign)) +
  scale_color_manual(values=c("blue","red"))+
  geom_linerange(stat = "identity",
                position = "identity",size=0.5,show.legend = F)+
  labs(x="Date",y=expression("Temperature Anomaly ("*degree*"C)"))+
  scale_x_date(date_breaks="2 year",date_labels=format("%Y"),
               limits=c(as.Date("2007-01-01"),as.Date("2023-12-31")))+
  geom_hline(aes(yintercept=0),color="black")+
  theme_bw()

ggsave(filename=paste0(basepath,"/figures/temperature_anomaly_",Sys.Date(),".png"),
       width=2000,height=714,units="px",dpi=300)



#################################################################################
###############################################################################
season_i = "MAM"
df_merged_short %>% mutate(Sign = if_else(Beam_temperature_corrected >= 0,
                                          "Positive", "Negative"))%>%
  filter(season==season_i) %>%
  ggplot(aes( ymin = 0,color = Sign)) +
  facet_grid(cols=vars(year))+
  scale_color_manual(values=c("blue","red"))+
  geom_linerange(aes_string("week","Beam_temperature_corrected",
                            ymax = "Beam_temperature_corrected"),stat = "identity",
                 position = "identity",size=0.5,show.legend = F)+
  labs(x="Week",y=expression("Temperature Anomaly ("*degree*"C)"))+
  ggtitle(paste(season_i,"Anomaly"))


#plot species anomaly
taxa = "Ditylum_brightwellii"
df_merged_short %>% mutate(Sign = if_else(!!sym(taxa)>= 0, "Positive", "Negative"))%>%
  ggplot(aes( ymin = 0,color = Sign)) +
  scale_color_manual(values=c("blue","red"))+
  geom_linerange(aes_string("date",taxa,
                            ymax = taxa),stat = "identity",
                 position = "identity",size=0.5,show.legend = F)+
  labs(x="Week",y=paste(taxa,"Concentration"))+
  ggtitle(paste(taxa,"Anomaly"))

taxa = "Ditylum_brightwellii"
df_merged_short %>% mutate(Sign = if_else(!!sym(taxa)>= 0, "Positive", "Negative"))%>%
  filter(season==season_i) %>%
  ggplot(aes( ymin = 0,color = Sign)) +
  facet_grid(cols=vars(year))+
  scale_color_manual(values=c("blue","red"))+
  geom_linerange(aes_string("week",taxa,
                            ymax = taxa),stat = "identity",
                 position = "identity",size=0.5,show.legend = F)+
  labs(x="Week",y=paste(taxa,"Concentration"))+
  ggtitle(paste(taxa, season_i,"Anomaly"))


df_merged_short_anomaly %>% mutate(diatom = rowSums(across(diatoms_list_include_maybe$species))) %>%
  filter(season=="DJF") %>%
  ggplot() +
  geom_hline(yintercept=0,color="red")+
  geom_vline(xintercept=0,color="red")+
  geom_point(aes_string(x="Beam_temperature_corrected",y="diatom")) +
  labs(x="Temperature Anomaly",y=paste("Diatom","Anomaly"))+
  geom_smooth(aes_string(x="Beam_temperature_corrected",y="diatom"),method="gam")

cor(df_merged_short_anomaly$Beam_temperature_corrected,df_merged_short_anomaly$Acantharia)


df_merged_short %>%
  mutate(diatom = rowSums(across(diatoms_list_include_maybe$species))) %>%
  mutate(Sign = if_else(diatom>= 0, "Positive", "Negative"))  %>% filter(Sign!="NA") %>%ggplot() +
  geom_boxplot(aes(x=Sign,y=diatom)) +
  labs(x="Temperature Anomaly",y=paste("Diatom","Anomaly"))+
  geom_signif(aes(x=Sign,y=diatom),
              comparisons=list(c("Negative","Positive")))


df_merged_short %>% mutate(dino = rowSums(across(dino_list_include_maybe$species))) %>% ggplot() +
  geom_hline(yintercept=0,color="red")+
  geom_vline(xintercept=0,color="red")+
  geom_point(aes_string(x="Beam_temperature_corrected",y="dino")) +
  labs(x="Temperature Anomaly",y=paste("Dinoflagellate","Anomaly"))


df_merged_short %>%
  mutate(dino = rowSums(across(dino_list_include_maybe$species))) %>%
  mutate(Sign = if_else(dino>= 0, "Positive", "Negative")) %>%
  filter(Sign!="NA") %>%
  ggplot() +
  geom_boxplot(aes_string(x="Sign",y="dino")) +
  labs(x="Temperature Anomaly",y=paste("Dino","Anomaly"))+
  geom_signif(aes(x=Sign,y=dino),
              comparisons=list(c("Negative","Positive")))




##################################################################
####################################################################

for (taxa in label_maybe_include){
  print(taxa)
df_merged_short %>% mutate(Sign = if_else(!!sym(taxa)>= 0, "Positive", "Negative"))%>%
  ggplot(aes( ymin = 0,color = Sign)) +
  scale_color_manual(values=c("blue","red"))+
  geom_linerange(aes_string("date",taxa,
                            ymax = taxa),stat = "identity",
                 position = "identity",size=0.5,show.legend = F)+
  labs(x="Week",y=paste(taxa,"Concentration"))+
  scale_x_date(date_breaks="1 year",date_labels=format("%Y"))+
  ggtitle(paste(taxa,"Anomaly"))
ggsave(filename=paste0(basepath,"/figures/anomaly/time_series/anomaly_time_series_",taxa,".png"),
       width=1800,height=600,units="px",dpi=150)
}

#################################################################################
#LOOPS
###################################################################################
#temp anomaly vs. taxa anomaly
for(taxa in label_maybe_include){
  print(taxa)
  df_merged_short %>% ggplot() +
    geom_hline(yintercept=0,color="red")+
    geom_vline(xintercept=0,color="red")+
    geom_point(aes_string(x="Beam_temperature_corrected",y=taxa)) +
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
taxa_i= "Acantharia"
for(taxa_i in label_maybe_include){
  print(taxa_i)
  min = pmin(-quadroot(abs(min(df_merged_short$Beam_temperature_corrected,na.rm=T))),-quadroot(abs(min(df_merged_short[,taxa_i],na.rm=T))))
  max = pmax(quadroot(abs(max(df_merged_short$Beam_temperature_corrected,na.rm=T))),quadroot(max(df_merged_short[,taxa_i],na.rm=T)))
  for(year_i in seq(2006,2023,1)){
    print(year_i)
    df_merged_short %>% filter(year==year_i) %>%
      mutate_at(label_maybe_include,quadroot) %>% ggplot(aes(x=week)) + 
      geom_hline(yintercept=0,color="blue")+
      geom_point(aes_string(y=taxa_i))+
      geom_point(aes(y=Beam_temperature_corrected),color="red",shape=4,size=5)+
      scale_y_continuous(name="Quadroot Transformed Carbon Concentration Anomaly",limits=c(min,max),sec.axis=sec_axis(~.*1,name=expression("Temperature Anomaly ("*degree*"C)"))) +
      theme(axis.title.y = element_text(color = "black"),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(colour = "red"))+
      scale_x_continuous(breaks=seq(1,52,4))+
       ggtitle(paste(year_i,taxa_i,"and Temperature"))
    ggsave(filename=paste0(basepath,"/figures/anomaly/yearly_species/",taxa_i,"/anomaly_",taxa_i,"_",year_i,".png"),
           width=800,height=800,units="px",dpi=150)
  }
}

taxa
for(taxa_i in label_maybe_include){
  print(taxa_i)
df_merged_short_anomaly %>%
  mutate(Sign = if_else(Beam_temperature_corrected>= 0, "Positive", "Negative"))%>%
  filter(Sign!="NA") %>%
  ggplot() +
  geom_boxplot(aes_string(x="Sign",y=taxa_i))+
  labs(x=expression("Temperature Anomaly"),y=paste(taxa_i,"Anomaly"))+
  geom_signif(aes_string(x="Sign",y=taxa_i),
              comparisons=list(c("Negative","Positive")),
              map_signif_level = TRUE,test="t.test")

ggsave(filename=paste0(basepath,"/figures/anomaly/correlation_sign_temp/anomaly_sign_temperature_",taxa_i,".png"),
       width=800,height=800,units="px",dpi=150)
}


for(taxa_i in label_maybe_include){
  print(taxa_i)
  df_merged_short_anomaly %>%
    mutate(Sign = if_else(Beam_temperature_corrected>= 0, "Positive", "Negative"))%>%
    filter(Sign!="NA",season=="JJA") %>%
    ggplot() +
    geom_boxplot(aes_string(x="Sign",y=taxa_i))+
    labs(x=expression("Temperature Anomaly"),y=paste(taxa_i,"Anomaly"))+
    geom_signif(aes_string(x="Sign",y=taxa_i),
                comparisons=list(c("Negative","Positive")),
                map_signif_level = TRUE,test="t.test")
  
  ggsave(filename=paste0(basepath,"/figures/anomaly/correlation_sign_temp_summer/anomaly_sign_temperature_",taxa_i,".png"),
         width=800,height=800,units="px",dpi=150)
}

for(taxa_i in label_maybe_include){
  print(taxa_i)
  df_merged_short_anomaly %>%
    mutate(Sign = if_else(Beam_temperature_corrected>= 0, "Positive", "Negative"))%>%
    filter(Sign!="NA",season=="DJF") %>%
    ggplot() +
    geom_boxplot(aes_string(x="Sign",y=taxa_i))+
    labs(x=expression("Temperature Anomaly"),y=paste(taxa_i,"Anomaly"))+
    geom_signif(aes_string(x="Sign",y=taxa_i),
                comparisons=list(c("Negative","Positive")),
                map_signif_level = TRUE,test="t.test")
  
  ggsave(filename=paste0(basepath,"/figures/anomaly/correlation_sign_temp_winter/anomaly_sign_temperature_",taxa_i,".png"),
         width=800,height=800,units="px",dpi=150)
}

