#PURPOSE: Process raw data

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate","dplyr","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#read in files from read_in_mvco_ifcb_files.R
data_path = paste0(basepath,"data/raw_mvco_ifcb/CSVs/2023_11_MVCO/")


#note df_stat_updated comes from a google sheet called optthresh
opt_thresh <- read.table(paste0(basepath,"data/df_stat_updated.tsv"),sep="\t",header =T)
index_maybe_include = which(opt_thresh$Updated.Status %in% c("maybe","include"))
index_include = which(opt_thresh$Updated.Status =="include")
label_maybe_include <-opt_thresh$Class[index_maybe_include]
label_include <-opt_thresh$Class[index_include]

save(opt_thresh,label_maybe_include,label_include,
     file=paste0(basepath,"data/r_objects/df_stat_opt_thresh",Sys.Date(),".RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))

#these come from the mat files from the classifier output
df_carbon <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_carbonC.csv"))
protist_tricho_label <-read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_protist_tricho_label.csv"),header=F)$V1
diatom_label <-read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_diatom_label.csv"),header=F)$V1
nfg_label <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_nanoflagcocco_label.csv"),header=F)$V1 %>%
                    append(c("Parvicorbicula_socialis","Phaeocystis"))
metazoan_label <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_metazoan_label.csv"),header=F)$V1
dino_label <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_dino_label.csv"),header=F)$V1
ciliate_label <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_ciliate_label.csv"),header=F)$V1
metadata <- read.csv(paste0(data_path,"carbon_conc_MVCO_2023_11_metadata.csv"),header=T)


df_carbon$date <- as.Date(metadata$datetime,format="%d-%b-%Y %H:%M:%S")
head(df_carbon)

syn_url <- "/home/mira/MIT-WHOI/data/2023/MVCO_syn_euk_conc_2023_Mar.csv"
df_syn_euk <- read.csv(syn_url)
df_syn_euk$date <- as.Date(df_syn_euk$Time_UTC,format = "%d-%b-%Y %H:%M:%S")
head(df_syn_euk)

#computing daily mean
df_syn_euk_daily <- df_syn_euk %>% group_by(date) %>% 
  summarise(Synechococcus = mean(sum_syn_biovol_perml,na.rm=T),
            Pico_eukaryotes = mean(sum_euk_biovol_perml,na.rm=T)) %>%
  drop_na()

list_remove <- c("Guinardia_delicatula","Guinardia_delicatula_TAG_internal_parasite",
                 "Chaetoceros_didymus","Chaetoceros_didymus_TAG_external_flagellate",
                 "Thalassiosira_TAG_external_detritus",
                 "Cylindrotheca","Cylindrotheca_morphotype1",
                 "Dinophysis_acuminata","Dinophysis_norvegica","Dinophysis_tripos",
                 "Chaetoceros","Chaetoceros_danicus","Chaetoceros_peruvianis","Chaetoceros_similis",
                   "Chaetoceros_socialis","Chaetoceros_subtilis","Chaetoceros_tenuissimus", 
                   "Chaetoceros_throndsenii","Chaetoceros_didymus","Chaetoceros_didymus_TAG_external_flagellate",
                 "Chrysochromulina","Chrysochromulina_lanceolata",
                 "coccolithophorid","Emiliania_huxleyi",
                 "pennate_Pseudo_nitzschia","Pseudo_nitzschia",
                 "Thalassionema","pennate_Thalassionema",
                 "Dinophyceae","Scrippsiella",
                 "pennate_Pseudo-nitzschia","Pseudo-nitzschia",
                 "pennate_Pseudo_nitzschia","Pseudo_nitzschia","Tintinnina","Tintinnopsis",
                 "Strombidium_capitatum","Strombidium_conicum","Strombidium_inclinatum",
                 "Strombidium_morphotype1","Strombidium_morphotype2","Strombidium_tintinnodes",
                 "Strombidium_wulffi")
list_add <- c("Guinardia_delicatula_merged","Cylindrotheca_merged",
              "Chaetoceros_merged", "Chrysochromulina_merged", "coccolithophorid_merged",
              "Pseudo_nitzschia_merged","Thalassionema_merged","Dinophyceae_merged",
              "Tintinnina_merged","Strombidium_merged","Dinophysis_merged","Synechococcus","Pico_eukaryotes")
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
         Tintinnina_merged = Tintinnina + Tintinnopsis,
         Strombidium_merged = Strombidium_capitatum + Strombidium_conicum +
           Strombidium_inclinatum+Strombidium_morphotype1 + Strombidium_morphotype2 +
          Strombidium_tintinnodes+Strombidium_wulffi) %>%
          select(-any_of(list_remove)) %>%
          full_join(df_syn_euk_daily,by="date")

df_carbonC$doy_numeric <- yday(df_carbonC$date)

names(df_carbonC)

#add and remove
protist_tricho_labelC <- protist_tricho_label %>%  setdiff(list_remove) %>% append(list_add)
diatom_labelC <- diatom_label %>% setdiff(list_remove) %>% append(list_add) 
diatom_labelC <- diatom_labelC[! diatom_labelC %in% c("Synechococcus","Pico_eukaryotes")] 

###### QC CHECKS
setdiff(protist_tricho_labelC,opt_thresh$Class) #should have 2 syn and pico euks
setdiff(opt_thresh$Class,protist_tricho_labelC) #should be 0
setdiff(protist_tricho_labelC,names(df_carbonC))# should be 0
setdiff(names(df_carbonC),protist_tricho_labelC) # should contain artifacts, dates, nonliving, not protist


save(protist_tricho_labelC,diatom_labelC,ciliate_label,dino_label,nfg_label,metazoan_label,
     file=paste0(basepath,"data/r_objects/unfilled/",Sys.Date(),"_df_carbon_labels.RData"))
save(metadata, file=paste0(basepath,"data/r_objects/unfilled/",Sys.Date(),"_df_carbon_meta_data.RData"))
save(df_carbonC,file=paste0(basepath,"data/r_objects/unfilled/",Sys.Date(),"_df_carbonC.RData"))


#reading in data
#############################################################################
#
###############################################################################

#Create filled version
df_carbonC_filled <- df_carbonC %>% 
  group_by(date) %>%
  summarize(across(all_of(protist_tricho_labelC),mean)) %>%
  #set to daily frequency
  complete(date = seq.Date(min(df_carbonC$date),max(df_carbonC$date), by="day")) %>%
  #fill out doy_numeric
  mutate(doy_numeric = yday(date)) %>%
  group_by(doy_numeric)%>%
  #replace nans for living things with yearly mean
  mutate(across(protist_tricho_labelC,~replace_na(.,mean(.,na.rm=T)))) %>%
  select(all_of(c(protist_tricho_labelC,"date","doy_numeric")))

save(df_carbonC_filled,file=paste0(basepath,"data/r_objects/filled/",Sys.Date(),"_df_carbonC_filled_merged.RData"))

