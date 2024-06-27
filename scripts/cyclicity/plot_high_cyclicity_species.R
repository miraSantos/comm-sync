basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
load(paste0(basepath,"data/r_objects/unfilled/2024-06-05_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-05_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/c_index_merged_df_cor_2024-06-05.RData"))


df_carbonC$week <- week(df_carbonC$date)
week_means <- df_carbonC %>% 
  group_by(week) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

##########################################
# cyclity - diatoms high
#############################################

diatoms_list_include_maybe <- c_index %>% filter(species %in% label_maybe_include,max_xcorr>=0.7,func_group=="Diatom") %>% 
  select(species) %>% unique()
diatoms_list_include <- c_index %>% filter(species %in% label_include,max_xcorr>=0.7,func_group=="Diatom") %>% 
  select(species) %>% unique()

dino_list_include_maybe <- c_index %>%  filter(species %in% label_maybe_include,max_xcorr>=0.6,func_group=="Dinoflagellate") %>% 
  select(species) %>% unique()
dino_list_include <- c_index %>%  filter(species %in% label_include,max_xcorr>=0.5,func_group=="Dinoflagellate") %>% 
  select(species) %>% unique()

high_c_index<- c_index %>%  filter(species %in% label_maybe_include,max_xcorr>=0.7) %>% 
  select(species) %>% unique()


ciliate_list_include_maybe <- c_index %>%  filter(species %in% label_maybe_include,max_xcorr>=0.55,func_group=="Ciliate") %>% 
  select(species) %>% unique()


mn_list_include_maybe <- c_index %>%  filter(species %in% label_maybe_include,max_xcorr>=0.6,func_group=="Misc. Nanoplankton") %>% 
  select(species) %>% unique()

week_means %>% 
  select(high_c_index$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + 
  geom_line(aes(x=week,y=conc,color=species)) +
  geom_point(aes(x=week,y=conc,color=species)) +
  labs(color = "Species",shape="Species")+
  scale_shape_manual(values=rainbow(14)) +
  xlab("Week")+
  scale_y_log10(label=comma)+
  scale_x_continuous(breaks=seq(0,53,4)) + 
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/high_cyclicity/high_cyclity_weekly_conc_",Sys.Date(),".png"),
       width=1800,height = 800,units = "px",dpi=150)

df_group_week <-df_group %>% group_by(week) %>% summarise_at(c("Diatom_noDetritus","Dinoflagellate","Ciliate","NanoFlagCocco"),mean,na.rm=T)


week_means %>% 
  select(diatoms_list_include_maybe$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + 
  # geom_point(data=df_group_week,aes(x=week,y=Diatom_noDetritus),color="black",alpha=0.2)+
  geom_line(aes(x=week,y=conc,color=species)) +
  geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  labs(color = "Species",shape="Species")+
  scale_shape_manual(values=1:length(diatoms_list_include_maybe$species)) +
  xlab("Week")+
  scale_y_log10(label=comma)+
  scale_x_continuous(breaks=seq(0,53,4)) + 
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray")
  )
 
ggsave(filename=paste0(basepath,"/figures/cyclic_index/mean_diatom_group_high_cyclicity_index_include",Sys.Date(),".png"),
       width=1400,height=800,units="px",dpi=200)




 week_means %>% 
  select(dino_list_include_maybe$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() +
   geom_line(aes(x=week,y=conc,color=species,shape=species)) +
   geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  labs(color = "Species",shape="Species")+
  xlab("Week")+
  scale_y_sqrt(label=comma)+
  scale_shape_manual(values=1:length(dino_list_include_maybe$species)) +
  scale_x_continuous(breaks=seq(0,53,4)) + 
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray")
  )


ggsave(filename=paste0(basepath,"/figures/cyclic_index/mean_dino_group_high_cyclicity_index",Sys.Date(),".png"),
       width=1400,height=800,units="px",dpi=200)



week_means %>% 
  select(ciliate_list_include_maybe$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + 
  geom_line(aes(x=week,y=conc,color=species,shape=species)) +
  geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  labs(color = "Species",shape="Species")+
  xlab("Week")+
  scale_y_sqrt(label=comma)+
  scale_shape_manual(values=1:length(ciliate_list_include_maybe$species)) +
  scale_x_continuous(breaks=seq(0,53,4)) + 
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/mean_ciliate_group_high_cyclicity_index",Sys.Date(),".png"),
       width=1400,height=800,units="px",dpi=200)



week_means %>% 
  select(mn_list_include_maybe$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() +
  geom_line(aes(x=week,y=conc,color=species,shape=species)) +
  geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  labs(color = "Species",shape="Species")+
  xlab("Week")+
  scale_y_sqrt(label=comma)+
  scale_shape_manual(values=1:length(ciliate_list_include_maybe$species)) +
  scale_x_continuous(breaks=seq(0,53,4)) + 
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/mean_misc_nano_group_high_cyclicity_index",Sys.Date(),".png"),
       width=1400,height=800,units="px",dpi=200)



week_means_long <-week_means %>%
  pivot_longer(-c("week"),names_to="taxa",values_to="conc")

annual_peak_times <- week_means_long %>% group_by(taxa) %>% summarise(annual_peak_time = week[which.max(conc)])

annual_peak_times %>% filter(taxa %in% dino_list_include_maybe$species) 

week_means %>% 
  select(dino_list_include_maybe$species,"week") %>%
  select(-c("Proterythropsis","Gyrodinium"))%>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  labs(color = "Species",shape="Species")+
  xlab("Week")+
  scale_y_sqrt(label=comma)+
  scale_shape_manual(values=1:length(dino_list_include_maybe$species)) +
  scale_x_continuous(breaks=seq(0,53,4)) + 
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray")
  )


week_means %>% 
  select(dino_list_include_maybe$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  labs(color = "Species",shape="Species")+
  xlab("Week")+
  scale_y_sqrt(label=comma)+
  scale_shape_manual(values=1:length(dino_list_include_maybe$species)) +
  scale_x_continuous(breaks=seq(0,53,4)) + 
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray")
  )


df_carbonC_wyear_mean %>% select(all_of(c(diatoms_list_include_maybe$species,
                                          "date","wyear","doy_numeric","week","year"))) %>%
pivot_longer(-c("date","wyear","doy_numeric","week","year"),
                   names_to="taxa",values_to="conc")   %>% 
  ggplot() + geom_point(aes(x=date, y = conc, color=taxa))+
  scale_y_log10()
