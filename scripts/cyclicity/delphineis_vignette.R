#plot delpheneis and ephemera vignette

#PURPOSE: GENERATE WAVELET R DATA OBJECTS AND GENERATE WAVELET SPECTRA
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","moments","grDevices")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
source(paste0(basepath,"/scripts/wavelet/plot_single_wt_arc.R"))



#load in super res
load(file=paste0(basepath,"/data/r_objects/wavelet_output_species_2024_Mar_19_PAUL.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC_filled.RData"))


index = which(protist_tricho_labelC=="Delphineis")
plot_single_wt_arc(df=df_carbonC_filled,super_res[[index]],title=paste0("Wavelet Transform of ",protist_tricho_labelC[index]),
                   save_folder = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/single_wt_vignette/",
                   save_name=paste0("wt_transform_",protist_tricho_labelC[index],".png"),plot.phase=TRUE)
