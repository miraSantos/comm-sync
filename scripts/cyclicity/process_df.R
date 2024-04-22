#PURPOSE: Process raw data

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate","dplyr","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
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

list_remove <- c("Guinardia_delicatula",
                 "Guinardia_delicatula_TAG_internal_parasite",
                 "Chaetoceros_didymus",
                 "Chaetoceros_didymus_TAG_external_flagellate",
                 "Thalassiosira_TAG_external_detritus")

lookup <- c(pennate_Pseudo_nitzschia="pennate_Pseudo.nitzschia", Pseudo_nitzschia="Pseudo.nitzschia")
df_carbonC <- df_carbon %>% mutate(Guinardia_delicatula_merged = Guinardia_delicatula + Guinardia_delicatula_TAG_internal_parasite,
                                   Chaetoceros_didymus_merged = Chaetoceros_didymus + Chaetoceros_didymus_TAG_external_flagellate) %>%
            rename(all_of(lookup)) %>%
          select(-all_of(list_remove))

df_carbonC$doy_numeric <- yday(df_carbonC$date)

list_remove2 <- c("pennate_Pseudo-nitzschia","Pseudo-nitzschia")
list_add2 <- c("pennate_Pseudo_nitzschia","Pseudo_nitzschia")
protist_tricho_labelC <- append(protist_tricho_label[!protist_tricho_label %in% append(list_remove,list_remove2)],append(list_add,list_add2))
diatom_labelC <- append(diatom_label[!diatom_label %in% append(list_remove,list_remove2)],append(list_add,list_add2))

df_carbonC$date <- as.Date(df_carbon_metadata$sample_time, format="%Y-%m-%d %H:%M:%S")

save(protist_tricho_labelC,diatom_labelC,ciliate_label,dino_label,nanoflagcocco_label,metazoan_label,
     file=paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbon_labels.RData"))

save(df_carbonC,file=paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC.RData"))

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

save(df_carbonC_filled,file=
       paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC_filled.RData"))

