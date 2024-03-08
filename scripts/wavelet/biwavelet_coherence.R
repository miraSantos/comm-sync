load("/dos/MIT-WHOI/community_sychrony/scripts/comm_sync.RData")
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/dos/MIT-WHOI/community_sychrony/scripts/adv_biwavelet_packages.R")
rm(wt)


############################################
#computing coherence on MVCO carbon concentration 
load("/home/mira/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_26_comm_sync_df_index.RData")
i = 3
time_index = seq(1,nrow(dfcarbon_conc),1)
temp_raw = scale(dfcarbon_conc["temp_beam"])
dat_raw = scale(dfcarbon_conc[shared_list[i]])
temp = as.matrix(cbind(time_index,temp_raw))
dat = as.matrix(cbind(time_index,dat_raw))
res = wtc_arc(d1=dat,d2=temp,
              asig.level = c(0.95),
              psig.level = c(0.95),anrands=1)

res=wt_arc(dat)
plot_wtc_arc(res,title=paste0("Coherence between Temperature and ",shared_list[i]),
             save_folder="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/",
             save_name = paste0("coherence_temp_phase_",shared_list[i],".png"),
             plot.phase=TRUE)

############################################
#computing coherence on NES LTER carbon concentration 
load("/dos/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_07_dfcarbon_group.RData")
i = 1
time_index = seq(1,nrow(dfcarbon_group),1)
temp_raw = scale(dfcarbon_group["temp_beam"])
dat_raw = scale(dfcarbon_group[group_list[i]])
temp = as.matrix(cbind(time_index,temp_raw))
dat = as.matrix(cbind(time_index,dat_raw))

res = wt_arc(dat,s0=16,max.scale = 365*4)
plot.biwavelet_adv(res)

res = wtc_arc(d1=dat,d2=temp)

plot_wtc_arc(res,title=paste0("Coherence between Temperature and ",group_list[i]),
                   save_folder="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/",
                   save_name = paste0("coherence_temp_phase_",group_list[i],".png"),
                   plot.phase=TRUE)


save(df_freq,all_index,file="/dos/MIT-WHOI/community_sychrony/data/comm_sync.RData")


#################
load("/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/temp_wtc_arc_protist_tricho.RData")
load("/dos/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_10_dfcarbon_group.RData")

plot_wtc_arc(dfcarbon_group,res,title=paste0("Coherence between Temperature and ","Protist Tricho"),
             save_folder="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/",
             save_name = paste0("coherence_temp_phase_Protist_Tricho",".png"),
             plot.phase=TRUE)
