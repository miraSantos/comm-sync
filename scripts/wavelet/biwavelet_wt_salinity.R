basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
load(paste0(basepath,"data/r_objects/unfilled/2023_Jul_26_dfcarbon_group.RData"))
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
source(paste0(basepath,"/scripts/wavelet/biwavelet_wt.R"))
rm(wt)

#SALINITY
load(paste0(basepath,"/data/r_objects/unfilled/salinity_2023_Oct_31.RData"))

head(salt_merged_agg)

df_sal_filled = fill_gaps(salt_merged_agg,"salt_mean")
df_sal_filled$year = year(df_sal_filled$date)
df_sal_filled <- df_sal_filled %>% filter(year >= 2006)

ggplot(df_sal_filled) + geom_point(aes(x=date,y= salt_mean))+
  ylab("Salinity (psu)")+xlab("Time (years)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")

ggsave(filename=paste0(basepath,"/figures/environmental/salinity/salinity_mvco_2006_2023.png"),
       width=2000,height=500,units="px",dpi=175)

time_index = seq(1,nrow(df_sal_filled),1)
dat = as.matrix(cbind(time_index,df_sal_filled$salt_mean))

res = wt_arc(dat,
             s0=16,
             max.scale = 365*4,
             asig.level = c(0.85),
             psig.level=c(0.85),
             anrands=500)

plot_single_wt_arc(df_sal_filled,
                   res,
                   title="",
                   save_folder=paste0(basepath,"/figures/wavelet_transforms/"),
                   save_name="salinity_wt_corrected.png")

plot.biwavelet_adv(res)



save_folder=paste0(basepath,"/figures/wavelet_transforms/")
save_name="salinity_wt_corrected.png"
