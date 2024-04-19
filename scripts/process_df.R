basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                        "ggplot2","tibbletime","dplyr")
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

list_add <- c("Guinardia_delicatula_merged",
              "Chaetoceros_didymus_merged")


df_carbon %>% mutate(Chaetoceros_didymus_merged = Chaetoceros_didymus + Chaetoceros_didymus_TAG_external_flagellate)

df_carbon %>% select(Guinardia_delicatula,Guinardia_delicatula_TAG_internal_parasite) %>% mutate(Guinardia_delicatula_merged = as.numeric(Guinardia_delicatula) + as.numeric(Guinardia_delicatula_TAG_internal_parasite))

df_carbonC <- df_carbon %>% mutate(Guinardia_delicatula_merged = rowSums(Guinardia_delicatula,Guinardia_delicatula_TAG_internal_parasite,na.rm=T),
                                   Chaetoceros_didymus_merged =rowSums(Chaetoceros_didymus,Chaetoceros_didymus_TAG_external_flagellate,na.rm=T)) 


# %>%select(-all_of(list_remove))

df_carbonC$doy_numeric <- yday(df_carbonC$date)

list_remove2 <- c("pennate_Pseudo-nitzschia","Pseudo-nitzschia")
list_add2 <- c("pennate_Pseudo.nitzschia","Pseudo.nitzschia")
protist_tricho_labelC <- append(protist_tricho_label[!protist_tricho_label %in% append(list_remove,list_remove2)],append(list_add,list_add2))
diatom_labelC <- append(diatom_label[!diatom_label %in% append(list_remove,list_remove2)],append(list_add,list_add2))


df_carbon$date <- as.Date(df_carbon_metadata$sample_time, format="%Y-%m-%d %H:%M:%S")

save(protist_tricho_labelC,diatom_labelC,ciliate_label,dino_label,nanoflagcocco_label,metazoan_label,
     file=paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbon_labels.RData"))

save(df_carbonC,
     file=paste0(basepath,"data/r_objects/unfilled/2023_Apr_17_df_carbonC.RData"))
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