basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
rm(wt)
source(paste0(basepath,"/scripts/wavelet/plot_wtc_arc.R"))

############################################
#computing coherence on NES LTER carbon concentration 

dfcarbon_group_merged= merge(dfcarbon_group,df_sal_filled)

i = 1
time_index = seq(1,nrow(dfcarbon_group_merged),1)
salt = scale(dfcarbon_group_merged["mean_salt"])
dat_raw = scale(dfcarbon_group_merged[group_list[i]])
env = as.matrix(cbind(time_index,salt))
dat = as.matrix(cbind(time_index,dat_raw))

res = wtc_arc(d1=dat,d2=env,
              s0=16,
              max.scale = 365*4,
              asig.level = c(0.95),
              psig.level=c(0.95),
              anrands=1000) 

plot_wtc_arc(dfcarbon_group_merged,res,title="",
             save_folder=paste0(basepath,"/figures/biwavelet_coherence/"),
             save_name = paste0("coherence_salt_phase_",group_list[i],".png"),
             plot.phase=TRUE)


for(i in 1:length(group_list)){print(i)
  time_index = seq(1,nrow(dfcarbon_group_merged),1)
  salt = scale(dfcarbon_group_merged["mean_salt"])
  dat_raw = scale(dfcarbon_group_merged[group_list[i]])
  env = as.matrix(cbind(time_index,salt))
  dat = as.matrix(cbind(time_index,dat_raw))
  
  res = wtc_arc(d1=dat,d2=env,
                s0=16,
                max.scale = 365*4,
                asig.level = c(0.95),
                psig.level=c(0.95),
                anrands=1000) 
  
  plot_wtc_arc(dfcarbon_group_merged,res,title="",
               save_folder=paste0(basepath,"/figures/biwavelet_coherence/"),
               save_name = paste0("coherence_salt_phase_",group_list[i],".png"),
               plot.phase=TRUE)
  }


ggplot(df_sal_comb) + geom_point(aes(x=date,y=mean_salt))+
xlab("Time")+ylab("Salinity (psu)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")



ggsave(paste0(basepath,"figures/time_series/salinity_mvco.png"),
       width=1000,height=500,units="px",dpi=150)
