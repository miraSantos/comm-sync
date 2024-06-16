basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/2024-06-04_df_carbonC_filled_super_res_paul.RData"))


load(paste0(basepath,"/data/r_objects/c_index_df_cor_",Sys.Date(),".RData"))

df_env <- df_env %>% group_by(wyear) %>% summarise(mean_temp = mean(Beam_temperature_corrected))

annual_peak_timing_long <- annual_peak_timing %>%
  pivot_longer(cols=protist_tricho_labelC,names_to="taxa",values_to="timing_annual_peak") %>% 
  mutate(wyear = paste0(timing_annual_peak,"_",year)) %>%
  left_join(df_env[c("wyear","mean_temp")],by="wyear",relationship="many-to-many")


annual_peak_timing_long %>% filter(taxa %in% diatom_labelC,!(year %in% c(2019,2021))) %>%
  ggplot() + geom_point(aes(x=mean_temp,y=timing_annual_peak,color=taxa))

annual_peak_timing_long %>% filter(taxa %in% dino_label,!(year %in% c(2019,2021))) %>%
  ggplot() + geom_point(aes(x=mean_temp,y=timing_annual_peak,color=taxa))


annual_peak_timing_long %>% filter(taxa =="Corethron_hystrix",!(year %in% c(2019,2021)))%>%
  ggplot() + geom_point(aes(x=mean_temp,y=timing_annual_peak,color=taxa))

annual_peak_timing_long %>% filter(taxa =="Paralia_sulcata",!(year %in% c(2019,2021)))%>%
  ggplot() + geom_point(aes(x=mean_temp,y=timing_annual_peak,color=taxa))


annual_peak_timing_long %>% filter(taxa =="Nanoneis",!(year %in% c(2019,2021)))%>%
  ggplot() + geom_point(aes(x=mean_temp,y=timing_annual_peak,color=taxa))
