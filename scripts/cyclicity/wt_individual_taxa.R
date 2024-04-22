#PURPOSE: GENERATE WAVELET R DATA OBJECTS AND GENERATE WAVELET SPECTRA
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","moments","grDevices")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
source(paste0(basepath,"/scripts/wavelet/plot_single_wt_arc.R"))

rm(wt) #replaces wt with the correct biwavelet version (there are 2 versions)

load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC_filled.RData"))

super_res <-list()

for(i in 1:length(protist_tricho_labelC)){
print(paste(i,"of",length(protist_tricho_labelC)))
time_index = seq(1,nrow(df_carbonC_filled),1)
dat = as.matrix(cbind(time_index,df_carbonC_filled[protist_tricho_labelC[i]]^(1/4)))
res= wt_arc(dat,mother="morlet")
super_res[[i]]<-res
}

df_carbonC_filled$year <- year(df_carbonC_filled$date)

save(super_res,
     file=paste0(basepath,"data/r_objects/unfilled/2023_Apr_03_df_carbonC_super_res.RData"))
####################### compute individual wavelet
#find index of deseired species 
i = which(protist_tricho_labelC=="Tripos")
print(paste(i,"of",length(protist_tricho_labelC)))
df_filt <- df_carbonC_filled %>% filter(year <=2022)
time_index = seq(1,nrow(df_filt),1)
dat = as.matrix(cbind(time_index,df_filt[protist_tricho_labelC[i]]^(1/4)))
res= wt_arc(dat,mother="morlet",psig.level = c(0.95,0.99))

plot.biwavelet_adv(res)
df_carbonC$year <- year(df_carbonC$date)
plot(df_carbonC$date,df_carbonC$Tripos^(1/4))
df_carbonC %>% filter(year <=2022) %>% ggplot() + 
  geom_point(aes(x=doy_numeric,y=Tripos,color=as.factor(year)))

#PLOT INDIVIDUAL
load(paste0(basepath,"/data/r_objects/wavelet_output_species_2024_Mar_15.RData"))
plot_single_wt_arc(df=df_carbonC_filled,super_res[[1]],title=paste0("Wavelet Transform of ",protist_tricho_labelC[1]),
                   save_folder = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/wavelet_transforms/single wt/",
                   save_name=paste0("wt_transform_",protist_tricho_labelC[1],".png"),plot.phase=TRUE)

plot(df_carbonC_filled[[protist_tricho_labelC[1]]])


