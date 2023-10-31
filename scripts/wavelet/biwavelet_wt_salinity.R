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

load(paste0(basepath,"/data/r_objects/unfilled/salinity_2023_Oct_23.RData"))

later_salt <- dfcarbon_group %>% filter(year > 2019) %>% select(date,salinity_beam) %>%
  mutate( year = year(date))
colnames(later_salt)[which(names(later_salt) == "salinity_beam")] <- "mean_salt"


df_sal_agg <- df_sal %>% group_by(date) %>% summarise(mean_salt = mean(salt)) %>% 
  mutate(year=year(date)) %>% filter(year >= 2006)

df_sal_comb <- rbind(df_sal_agg,later_salt)

df_sal_filled = fill_gaps(df_sal_comb,"mean_salt")


ggplot(df_sal_filled) + geom_point(aes(x=date,y= mean_salt))+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")

time_index = seq(1,nrow(df_sal_filled),1)
dat = as.matrix(cbind(time_index,df_sal_filled$mean_salt))

res = wt_arc(dat,
             s0=16,
             max.scale = 365*4,
             asig.level = c(0.95),
             psig.level=c(0.95),
             anrands=1000)

plot_single_wt_arc(df_sal_filled,
                   res,
                   title="",
                   save_folder=paste0(basepath,"/figures/wavelet_transforms/"),
                   save_name="salinity_wt_corrected.png")

plot.biwavelet_adv(res)



save_folder=paste0(basepath,"/figures/wavelet_transforms/")
save_name="salinity_wt_corrected.png"
