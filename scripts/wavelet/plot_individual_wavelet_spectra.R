
#PURPOSE:Plot individual wavelet spectra
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
source(paste0(basepath,"/scripts/wavelet/plot_single_wt_arc.R"))

load(super_res,protist_tricho_labelC,file=paste0(basepath,"/data/r_objects/wavelet_output_species_2024_Mar_22_morlet.RData"))

for (i in 1:length(protist_tricho_labelC)){
  print(i)
  plot_single_wt_arc(df=df_carbonC_filled,super_res[[i]],title=paste0("Wavelet Transform of ",protist_tricho_labelC[i]),
                     save_folder = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/wavelet_transforms/single_wt_morlet/",
                     save_name=paste0("wt_transform_",protist_tricho_labelC[i],".png"))
}

