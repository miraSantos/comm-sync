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

diatoms_list_include_maybe <- c_index_merged %>% filter(species %in% label_maybe_include,cyclicity_index>=0.6,func_group=="Diatom") %>% 
  select(species) %>% unique()
diatoms_list_include <- c_index_merged %>% filter(species %in% label_maybe_include,cyclicity_index>=0.6,func_group=="Diatom") %>% 
  select(species) %>% unique()

dino_list_include_maybe <- c_index_merged %>%  filter(species %in% label_maybe_include,cyclicity_index>=0.5,func_group=="Dinoflagellate") %>% 
  select(species) %>% unique()
dino_list_include <- c_index_merged %>%  filter(species %in% label_maybe_include,cyclicity_index>=0.5,func_group=="Dinoflagellate") %>% 
  select(species) %>% unique()

high_c_index_merged <- c_index_merged %>%  filter(species %in% label_maybe_include,cyclicity_index>=0.7) %>% 
  select(species) %>% unique()

week_means %>% 
  select(high_c_index_merged$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + 
  geom_line(aes(x=week,y=conc,color=species)) +
  geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  geom_smooth(aes(x=week,y=conc),method = "gam",color="black",label="gam_")+
  labs(color = "Species",shape="Species")+
  scale_shape_manual(values=1:length(high_c_index_merged$species)) +
  xlab("Week")+
  scale_y_sqrt(label=comma)+
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

week_means %>% 
  select(diatoms_list_include_maybe$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + 
  geom_line(aes(x=week,y=conc,color=species)) +
  geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  geom_smooth(aes(x=week,y=conc),method = "",color="black",label="Loess Mean")+
  labs(color = "Species",shape="Species")+
  scale_shape_manual(values=1:length(diatoms_list_include_maybe$species)) +
  xlab("Week")+
  scale_y_sqrt(label=comma)+
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

ggsave(filename=paste0(basepath,"/figures/cyclic_index_merged/mean_diatom_group_high_cyclicity_index_include.png"),
       width=1400,height=800,units="px",dpi=200)

 week_means %>% 
  select(dino_list$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  geom_smooth(aes(x=week,y=conc),method = "loess",color="black")+
  labs(color = "Species",shape="Species")+
  xlab("Week")+
  scale_y_sqrt(label=comma)+
  scale_shape_manual(values=1:length(dino_list$species)) +
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


ggsave(filename=paste0(basepath,"/figures/cyclic_index_merged/mean_dino_group_high_cyclicity_index.png"),
       width=1400,height=800,units="px",dpi=200)
