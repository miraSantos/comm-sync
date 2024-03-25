#PURPOSE: GENERATE WAVELET R DATA OBJECTS AND GENERATE WAVELET SPECTRA
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
source(paste0(basepath,"/scripts/wavelet/plot_single_wt_arc.R"))

rm(wt) #replaces wt with the correct biwavelet version (there are 2 versions)

#read in files from read_in_mvco_ifcb_files.R
data_path = paste0(basepath,"data/raw_mvco_ifcb/CSVs/2023_11_MVCO/")
carbonC_list <- list.files(data_path)

load(paste0(basepath,"data/r_objects/unfilled/2023_Mar_15_df_carbon.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2023_Mar_15_df_carbon_metadata.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2023_Mar_15_df_carbon_optthresh.RData"))

optthresh_nan = names(df_carbon_optthresh)[which(df_carbon_optthresh == "NaN")]

protist_tricho_label <- read.csv(paste0(data_path,carbonC_list[9]),header=F)$V1
diatom_label <- read.csv(paste0(data_path,carbonC_list[3]),header=F)$V1
dino_label <- read.csv(paste0(data_path,carbonC_list[4]),header=F)$V1
ciliate_label <-read.csv(paste0(data_path,carbonC_list[2]),header=F)$V1
nanoflagcocco_label <- read.csv(paste0(data_path,carbonC_list[7]),header=F)$V1
metazoan_label <- read.csv(paste0(data_path,carbonC_list[6]),header=F)$V1

df_carbon$date <- as.Date(df_carbon_metadata$sample_time, format="%Y-%m-%d %H:%M:%S")

list_remove <- c("Guinardia_delicatula",
  "Guinardia_delicatula_TAG_internal_parasite",
  "Chaetoceros_didymus",
  "Chaetoceros_didymus_TAG_external_flagellate",
  "Thalassiosira",
  "Thalassiosira_TAG_external_detritus")

list_add <- c("Guinardia_delicatula_merged",
              "Chaetoceros_didymus_merged",
              "Thalassiosira_merged")


df_carbonC <- df_carbon %>% mutate(Guinardia_delicatula_merged = Guinardia_delicatula + Guinardia_delicatula_TAG_internal_parasite,
                      Chaetoceros_didymus_merged = Chaetoceros_didymus+ Chaetoceros_didymus_TAG_external_flagellate,
                      Thalassiosira_merged = Thalassiosira+Thalassiosira_TAG_external_detritus) %>%
                select(-all_of(list_remove))

list_remove2 <- c("pennate_Pseudo-nitzschia","Pseudo-nitzschia")
list_add2 <- c("pennate_Pseudo.nitzschia","Pseudo.nitzschia")
protist_tricho_labelC <- append(protist_tricho_label[!protist_tricho_label %in% append(list_remove,list_remove2)],append(list_add,list_add2))
diatom_labelC <- append(diatom_label[!diatom_label %in% append(list_remove,list_remove2)],append(list_add,list_add2))

df_carbonC_filled <- df_carbonC %>% 
  group_by(date) %>%
  summarize(across(protist_tricho_labelC,mean)) %>%
  #set to daily frequency
  complete(date = seq.Date(min(df_carbonC$date),max(df_carbonC$date), by="day")) %>%
  #fill out doy_numeric
  mutate(doy_numeric = yday(date)) %>%
  group_by(doy_numeric)%>%
  #replace nans for living things with yearly mean
  mutate(across(protist_tricho_labelC,~replace_na(.,mean(.,na.rm=T)))) %>%
  select(all_of(c(protist_tricho_labelC,"date","doy_numeric")))


super_res <-list()

for(i in 1:length(protist_tricho_labelC)){
print(paste(i,"of",length(protist_tricho_labelC)))
time_index = seq(1,nrow(df_carbonC_filled),1)
dat = as.matrix(cbind(time_index,df_carbonC_filled[protist_tricho_labelC[i]]^(1/4)))
res= wt_arc(dat,mother="morlet")
super_res[[i]]<-res
}

#PLOT INDIVIDUAL
load(paste0(basepath,"/data/r_objects/wavelet_output_species_2024_Mar_15.RData"))
plot_single_wt_arc(df=df_carbonC_filled,super_res[[1]],title=paste0("Wavelet Transform of ",protist_tricho_labelC[1]),
                   save_folder = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/wavelet_transforms/single wt/",
                   save_name=paste0("wt_transform_",protist_tricho_labelC[1],".png"),plot.phase=TRUE)

plot(df_carbonC_filled[[protist_tricho_labelC[1]]])


