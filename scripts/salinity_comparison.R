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

