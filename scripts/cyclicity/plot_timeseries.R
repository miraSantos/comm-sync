
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","car","graphics","stats")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"/data/r_objects/c_index_df_cor_2024)22_april.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC.RData"))

head(df_carbonC)

for(jj in 1:length(opt_list_merged)){
  print(jj)
df_carbonC_wyear_mean %>% ggplot() +
  geom_line(aes_string(x="date",y=opt_list_merged[jj]))+
  scale_x_date(date_break="2 year",date_label="%Y")+
  scale_y_log10(limits=c(1,50000),labels = function(x) format(x, scientific = FALSE))+
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  xlab("Time")
  ggsave(filename=paste0(basepath,"/figures/annual_cycle/timeseries/timeseries_",opt_list_merged[jj],".png"),
       width=1400,height=800,units="px",dpi=200)
}
