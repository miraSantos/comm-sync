list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/dos/MIT-WHOI/community_sychrony/scripts/adv_biwavelet_packages.R")
rm(wt)
source("/home/mira/MIT-WHOI/github_repos/comm-sync/scripts/wavelet/plot_wtc_arc.R")

load("/home/mira/MIT-WHOI/github_repos/comm-sync/data/r_objects/filled/2023_Jul_26_dfcarbon_group.RData")

head(dfcarbon_class)
species_list <- names(dfcarbon_class)
#computing coherence on NES LTER carbon concentration 
i = 1
time_index = seq(1,nrow(dfcarbon_class),1)
dat1_raw = scale(dfcarbon_class[species_list[4]])
dat2_raw = scale(dfcarbon_class[species_list[3]])
dat1 = as.matrix(cbind(time_index,dat1_raw))
dat2 = as.matrix(cbind(time_index,dat2_raw))

res = wtc_arc(d1=dat1,d2=dat2,
              asig.level = c(0.95),
              psig.level = c(0.95),anrands=1)

res = wt_arc(dat)
plot.biwavelet_adv(res)

res = wtc_arc(d1=dat1,d2=dat2)

plot_wtc_arc(df=dfcarbon_class,res=res,title=paste0("Coherence between"),
             save_folder="/home/mira/MIT-WHOI/github_repos/comm-sync/figures/biwavelet_coherence/pairwise/",
             save_name = paste0("coherence_pairwise.png"),
             plot.phase=TRUE)


save(df_freq,all_index,file="/dos/MIT-WHOI/community_sychrony/data/comm_sync.RData")