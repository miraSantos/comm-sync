basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
load(paste0(basepath,"data/r_objects/unfilled/2023_Jul_26_dfcarbon_group.RData"))
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","ggbump")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
source(paste0(basepath,"/scripts/wavelet/biwavelet_wt.R"))
rm(wt)


#DINOFLAGELLATES
time_index = seq(1,nrow(dfcarbon_group),1)
dat = as.matrix(cbind(time_index,scale(dfcarbon_group$salinity_beam)))
res = wt_arc(dat,
             s0=16,
             max.scale = 365*4,
             asig.level = c(0.95),
             psig.level=c(0.95),
             anrands=1000)

res=wt_arc(dat)
plot.biwavelet_adv(res)

plot_single_wt_arc(dfcarbon_group,
                   res,
                   title="",
                   save_folder=paste0(basepath,"/figures/wavelet_transforms/"),
                   save_name="test_dino_2.png")
