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


df_env <- read.csv("/home/mira/MIT-WHOI/github_repos/comm-sync/data/mvco/mvco_env.csv")
df_env$date <- as.Date(df_env$days,format ="%d-%b-%Y")
df_env$year <- year(df_env$date)

df_env %>% filter(year>=2006) %>% ggplot() + geom_line(aes(x=date,y=Beam_temperature_corrected),size=1.2)+
  xlab("Time")+ylab("Temperature (Deg C)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")

df_env_filled_temp= fill_gaps(df_env,"Beam_temperature_corrected")


time_index = seq(1,nrow(df_env_filled_temp),1)
dat = as.matrix(cbind(time_index,df_env_filled_temp$Beam_temperature_corrected))

res = wt_arc(dat,
             s0=16,
             max.scale = 365*4,
             asig.level = c(0.95),
             psig.level=c(0.95),
             anrands=1000)

plot_single_wt_arc(df_env_filled_temp,
                   res,
                   title="",
                   save_folder=paste0(basepath,"/figures/wavelet_transforms/"),
                   save_name="temperature_wt_corrected.png")
