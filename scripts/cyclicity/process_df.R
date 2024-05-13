#PURPOSE: Process raw data

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate","dplyr","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#read in files from read_in_mvco_ifcb_files.R
data_path = paste0(basepath,"data/raw_mvco_ifcb/CSVs/2023_11_MVCO/")

load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))

df_carbon <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_carbonC.csv"))
protist_tricho_label <-read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_protist_tricho_label.csv"),header=F)$V1
diatom_label <-read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_diatom_label.csv"),header=F)$V1
nfg_label <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_nanoflagcocco_label.csv"),header=F)$V1
metazoan_label <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_metazoan_label.csv"),header=F)$V1
dino_label <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_dino_label.csv"),header=F)$V1
ciliate_label <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_ciliate_label.csv"),header=F)$V1
metadata <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_metadata.csv"),header=T)

df_carbon$date <- as.Date(metadata$datetime,format="%d-%b-%Y %H:%M:%S")
head(df_carbon)

list_remove <- c("Guinardia_delicatula","Guinardia_delicatula_TAG_internal_parasite",
                 "Chaetoceros_didymus","Chaetoceros_didymus_TAG_external_flagellate",
                 "Thalassiosira_TAG_external_detritus",
                 "Cylindrotheca","Cylindrotheca_morphotype1",
                 "Dinophysis_acuminata","Dinophysis_norvegica","Dinophysis_tripos",
                 "Chaeotoceros","Chaetoceros_danicus","Chaetoceros_peruvianis","Chaetoceros_similis",
                   "Chaetoceros_socialis","Chaetoceros_subtilis","Chaetoceros_tenuissimus", 
                   "Chaetoceros_throndsenii","Chaetoceros_didymus","Chaetoceros_didymus_TAG_external_flagellate",
                 "Chrysochromulina","Chrysochromulina_lanceolata",
                 "coccolithophorid","Emiliania_huxleyi",
                 "pennate_Pseudo_nitzschia","Pseudo_nitzschia",
                 "Thalassionema","pennate_Thalassionema",
                 "Dinophyceae","Scrippsiella",
                 "Tripos_furca","Tripos_fusus","Tripos_lineatus","Tripos",
                 "pennate_Pseudo-nitzschia","Pseudo-nitzschia",
                 "pennate_Pseudo_nitzschia","Pseudo_nitzschia")
list_add <- c("Guinardia_delicatula_merged","Cylindrotheca_merged",
              "Chaetoceros_merged", "Chrysochromulina_merged", "coccolithophorid_merged",
              "Pseudo_nitzschia_merged","Thalassionema_merged","Dinophyceae_merged","Tripos_merged")
lookup <- c("pennate_Pseudo_nitzschia"="pennate_Pseudo.nitzschia", "Pseudo_nitzschia"="Pseudo.nitzschia")
df_carbonC <- df_carbon %>% 
  rename(all_of(lookup)) %>%
  mutate(Guinardia_delicatula_merged = Guinardia_delicatula + Guinardia_delicatula_TAG_internal_parasite,
         Cylindrotheca_merged = Cylindrotheca + Cylindrotheca_morphotype1,
         Dinophysis_merged = Dinophysis_acuminata + Dinophysis_norvegica + Dinophysis_tripos,
         Chaetoceros_merged = Chaetoceros + Chaetoceros_danicus + Chaetoceros_peruvianis + Chaetoceros_similis + 
           Chaetoceros_socialis + Chaetoceros_subtilis + Chaetoceros_tenuissimus + 
           Chaetoceros_throndsenii + Chaetoceros_didymus + Chaetoceros_didymus_TAG_external_flagellate,
         Chrysochromulina_merged = Chrysochromulina + Chrysochromulina_lanceolata,
         coccolithophorid_merged = coccolithophorid + Emiliania_huxleyi,
         Pseudo_nitzschia_merged = pennate_Pseudo_nitzschia + Pseudo_nitzschia,
         Thalassionema_merged = Thalassionema + pennate_Thalassionema,
         Dinophyceae_merged = Dinophyceae + Scrippsiella,
         Tripos_merged = Tripos_furca + Tripos_fusus + Tripos_lineatus + Tripos) %>%
          select(-any_of(list_remove))

df_carbonC$doy_numeric <- yday(df_carbonC$date)

#add and remove
protist_tricho_labelC <- protist_tricho_label %>%  setdiff(list_remove) %>% append(list_add)
diatom_labelC <- diatom_label %>% setdiff(list_remove) %>% append(list_add)

save(protist_tricho_labelC,diatom_labelC,ciliate_label,dino_label,nfg_label,metazoan_label,
     file=paste0(basepath,"data/r_objects/unfilled/2024_May_13_df_carbon_labels.RData"))
save(metadata, file=paste0(basepath,"data/r_objects/unfilled/2024_May_13_df_carbon_meta_data.RData"))
save(df_carbonC,file=paste0(basepath,"data/r_objects/unfilled/2024_May_13_df_carbonC.RData"))

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

save(df_carbonC_filled,file=paste0(basepath,"data/r_objects/unfilled/2024_May_13_df_carbonC_filled.RData"))

