basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","moments")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-05_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-05_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh2024-06-27.RData"))
load(paste0(basepath,"/data/r_objects/c_index_df_cor_2024-06-27.RData"))

df_lag_long <- df_lag_xcorr %>%
  pivot_longer(-c("year"),names_to="taxa",values_to="lag")

df_max_long <- df_max_xcorr %>%
  pivot_longer(-c("year"),names_to="taxa",values_to="max_xcorr") %>%
  group_by(taxa) %>% reframe(laci = mean(max_xcorr,na.rm=T),max_xcorr=max_xcorr,year=year) %>%
  ungroup()

df_merge_lag_max<- merge(df_lag_long,df_max_long,by=c("taxa","year"))
str(df_merge_lag_max)
df_merge_lag_max %>% filter(taxa %in% label_maybe_include) %>%
  ggplot() +
  geom_tile(aes(x=year,y=reorder(taxa,+laci),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#5EC625", low="#A73ABD")+
  theme_bw()+
  labs(x="Year",y="Taxa",fill="Lag")+
  scale_x_continuous(breaks=seq(2006,2022,2))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/lag/lag_heatmap_vertical_",Sys.Date(),".png"),
       width=2000,height=3000,units="px",dpi=300)

df_merge_lag_max %>% filter(taxa %in% label_maybe_include) %>%
  ggplot() +
  geom_tile(aes(x=year,y=reorder(taxa,-laci),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#dc322f", low="#268bd2")+
  theme_bw()+
  coord_flip()+
  labs(x="Year",y="Taxa",fill="Lag")+
  scale_x_continuous(breaks=seq(2006,2022,2))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95))


ggsave(filename=paste0(basepath,"/figures/cyclic_index/lag/lag_heatmap_horizontal_",Sys.Date(),".png"),
       width=3600,height=1500,units="px",dpi=300)

##########################################
#plot functional groups
#########################################3
#diatoms
df_merge_lag_max %>% filter(taxa %in% diatom_labelC)%>%
  ggplot() +
  geom_tile(aes(y=year,x=reorder(taxa,-laci),fill=lag))+
  scale_fill_gradient2(midpoint = 0,
                       mid="#eee8d5", high="#dc322f", low="#268bd2")+
  theme_bw()+
  labs(y="Year",x="Taxa",fill="Lag")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
        plot.margin =unit(c(0,0,0,1),"cm"))+
  scale_y_continuous(breaks=seq(2006,2022,2))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/lag/lag_adjustment_diatoms_",Sys.Date(),".png"),
       width=2300,height=1200,units="px",dpi=300)

df_merge_lag_max %>% filter(taxa %in% dino_labelC)%>%
  ggplot() +
  geom_tile(aes(y=year,x=reorder(taxa,-laci),fill=lag))+
  scale_fill_gradient2(midpoint = 0,
                       mid="#eee8d5", high="#dc322f", low="#268bd2")+
  theme_bw()+
  labs(x="Year",y="Taxa",fill="Lag")+
  scale_y_continuous(breaks=seq(2006,2022,2))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
        plot.margin =unit(c(0,0,0,1),"cm"))+
  scale_y_continuous(breaks=seq(2006,2022,2))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/lag/lag_adjustment_dino_",Sys.Date(),".png"),
       width=2300,height=1200,units="px",dpi=300)

df_merge_lag_max %>% filter(taxa %in% ciliate_labelC)%>%
  ggplot() +
  geom_tile(aes(y=year,x=reorder(taxa,-laci),fill=lag))+
  scale_fill_gradient2(midpoint = 0,
                       mid="#eee8d5", high="#dc322f", low="#268bd2")+
  theme_bw()+
  labs(x="Year",y="Taxa",fill="Lag")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
        plot.margin =unit(c(0,0,0,1),"cm"))+
  scale_y_continuous(breaks=seq(2006,2022,2))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/lag/lag_adjustment_ciliate_",Sys.Date(),".png"),
       width=2300,height=1200,units="px",dpi=300)

df_merge_lag_max %>% filter(taxa %in% nfg_labelC)%>%
  ggplot() +
  geom_tile(aes(y=year,x=reorder(taxa,-laci),fill=lag))+
  scale_fill_gradient2(midpoint = 0,
                       mid="#eee8d5", high="#dc322f", low="#268bd2")+
  theme_bw()+
  labs(x="Year",y="Taxa",fill="Lag")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
        plot.margin =unit(c(0,0,0,1),"cm"))+
  scale_y_continuous(breaks=seq(2006,2022,2))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/lag/lag_adjustment_miscnano_",Sys.Date(),".png"),
       width=2300,height=1200,units="px",dpi=300)


#####################
#histograms
######################
df_lag_long %>% filter(taxa%in% diatom_labelC) %>% group_by(taxa) %>% ggplot()+
  geom_histogram(aes(x=lag))


df_lag_long %>% filter(taxa%in% dino_labelC) %>% group_by(taxa) %>% ggplot()+
  geom_histogram(aes(x=lag))

df_lag_long %>% filter(taxa%in% ciliate_labelC) %>% group_by(taxa) %>% ggplot()+
  geom_histogram(aes(x=lag))


df_lag_stats <-df_lag_long %>% group_by(taxa) %>% summarise(avg_lag_mag = mean(abs(lag),na.rm=T),avg_lag=mean(lag,na.rm=T),skew = skewness(lag,na.rm=T),kurtosis=kurtosis(lag,na.rm=T))

df_lag_stats_ordered <-df_lag_stats[order(df_lag_stats$avg_lag_mag,decreasing=F),]

df_lag_stats_merged <- left_join(df_lag_stats_ordered,c_index,by="taxa")

df_lag_stats_merged %>% ggplot() + geom_point(aes(y=avg_lag_mag,x=max_xcorr))+
  labs(x="LACI",y="Mean Lag Magnitude") 

cor.test(df_lag_stats_merged$avg_lag_mag,df_lag_stats_merged$max_xcorr)
