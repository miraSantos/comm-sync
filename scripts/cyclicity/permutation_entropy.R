basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","statcomp","dplyr","tidyr")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-12_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-12_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))


df_env <- read.csv("/home/mira/MIT-WHOI/Week.2024.05.19-25/mvco_temp_2024.csv")
df_env$date <- as.Date(df_env$timestamp,format ="%Y-%m-%dT%H:%M:%S")
df_env$year <- year(df_env$date)

df_env_merge <-merge(df_carbonC,df_env[,c("Beam_temperature_corrected","date")], by="date") %>%
  mutate(year = year(date),week=week(date),wyear=paste0(week(date),"-",year(date)))

df_merge_wyear_mean <-df_env_merge %>%
  group_by(wyear) %>%
  mutate_at(c(label_maybe_include,"Beam_temperature_corrected"),mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

opd = ordinal_pattern_distribution(x =df_env_merge$Beam_temperature_corrected,
                                   ndemb = 10)

opd = ordinal_pattern_distribution(x =runif(n=length(df_env_merge$Beam_temperature_corrected),min=0,max=300),
                                   ndemb = 10)
print(paste("Permutation Entropy =",permutation_entropy(opd)))


df_carbonC_wyear_long <- df_merge_wyear_mean %>%
  select(c(protist_tricho_labelC,"wyear","week","doy_numeric","year")) %>%
  pivot_longer(cols=protist_tricho_labelC,names_to=("taxa"),values_to="conc")


perm_ent <- df_carbonC_wyear_long %>% group_by(taxa) %>%  
  summarise(perm_ent=permutation_entropy(ordinal_pattern_distribution(x =conc, ndemb = 10)))

func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton","Metazoan","Cyanobacteria","Picoeukaryotes")
func_group_labels <- list(diatom_labelC,dino_label,ciliate_label,nfg_label,metazoan_label,c("Synechococcus","Trichodesmium"),c("Pico_eukaryotes"))
#create column with functional group 
for(func_group in 1:length(func_group_list)){
  reference=func_group_labels[[func_group]]
  perm_ent[perm_ent$taxa%in%reference,"func_group"] = func_group_list[func_group]
}

perm_ent %>% filter(taxa %in% label_maybe_include) %>% 
  ggplot() + geom_bar(aes(x=reorder(taxa,+perm_ent),y=perm_ent,fill=func_group),stat="identity")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  coord_flip()+
  labs(x="Taxa",y="Permutation Entropy")

