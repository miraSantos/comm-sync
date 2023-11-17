list.of.packages <- c("readxl","fields","ggplot2","ggpubr","dplyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

salt_merged_agg

wmg_coher

mbay_coher

colors <- c("MVCO" = "black", "WMG" = "green", "MA Bay" = "red")

ggplot()+geom_point(aes(x=salt_merged_agg$date,y=salt_merged_agg$salt_mean,color="MVCO"))+
  geom_point(aes(x=wmg_coher$date,y=wmg_coher$salt_mean,color="WMG"),alpha=0.4)+
  geom_point(aes(x=mbay_coher$date,y=mbay_coher$salt_mean,color="MA Bay"),alpha=0.2)+
  labs(x = "Year",
       y = "Salinity (psu)",
       color = "Legend") +
  scale_color_manual(values = colors)


corr(salt_merged_agg$salt_mean,wmg_coher)

ggplot()+
  geom_point(aes(x=wmg_coher$date,y=wmg_coher$salt_mean),alpha=0.4)+
  labs(x = "Year",
       y = "Salinity (psu)") +
  scale_x_date(date_breaks="2 year",date_labels = "%Y")


ggsave(filename=paste0(basepath,"/figures/environmental/salinity/summer-salinity-wmg.png"),
       width = 2000,height=500,units="px",dpi =175)


ggplot()+
  geom_point(aes(x=mbay_coher$date,y=mbay_coher$salt_mean),alpha=0.4)+
  labs(x = "Year",
       y = "Salinity (psu)") +
  scale_x_date(date_breaks="2 year",date_labels = "%Y")


ggsave(filename=paste0(basepath,"/figures/environmental/salinity/summer-salinity-mbay.png"),
       width = 2000,height=500,units="px",dpi =175)


#taking out mean and plotting anomaly
salt_merged_agg$doy_numeric <- yday(salt_merged_agg$date)
yday_mean <- salt_merged_agg %>% group_by(doy_numeric) %>% mutate(yday_mean = mean(salt_mean,na.rm=T)) %>% ungroup()
yday_mean$anomaly <- yday_mean$salt_mean - yday_mean$yday_mean


mbay_coher$doy_numeric <- yday(mbay_coher$date)
yday_mean_mbay <- mbay_coher %>% group_by(doy_numeric) %>% mutate(yday_mean = mean(salt_mean,na.rm=T)) %>% ungroup()
yday_mean_mbay$anomaly <- yday_mean_mbay$salt_mean - yday_mean_mbay$yday_mean

wmg_coher$doy_numeric <- yday(wmg_coher$date)
yday_mean_wmg <- wmg_coher %>% group_by(doy_numeric) %>% mutate(yday_mean = mean(salt_mean,na.rm=T)) %>% ungroup()
yday_mean_wmg$anomaly <- yday_mean_wmg$salt_mean - yday_mean_wmg$yday_mean



colors <- c("MVCO" = "black", "MA Bay" = "green", "Western Maine Gulf" = "blue")

ggplot() + geom_point(data=yday_mean,aes(x=date,y=anomaly,color="MVCO"))+
  geom_point(data=yday_mean_mbay,aes(x=date,y=anomaly,color="MA Bay"),alpha=0.1)+
    geom_point(data=yday_mean_wmg,aes(x=date,y=anomaly,color="Western Maine Gulf"),alpha=0.1)+
  geom_hline(yintercept = 0,color="red")+
  labs(x = "Year",
       y = "Salinity (psu)",
       color = "Legend") +
  scale_color_manual(values = colors)
  


dfsal_merged <- merge(salt_merged_agg,mbay_coher, by="date")

df_sal_merged_2 <-merge(dfsal_merged, wmg_coher, by="date")


df_sal_merged_2