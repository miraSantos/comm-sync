
week_means <- df_carbonC %>% 
  group_by(week) %>%
  summarize_at(opt_list_merged,mean,na.rm=T)

##########################################
# cyclity - diatoms high
#############################################

diatoms_list_include_maybe <- c_index %>% filter(species %in% opt_list_merged,cyclicity_index>=0.6,func_group=="Diatom") %>% 
  select(species) %>% unique()
diatoms_list_include <- c_index %>% filter(species %in% opt_list_include,cyclicity_index>=0.6,func_group=="Diatom") %>% 
  select(species) %>% unique()
diatoms_list$species

dino_list_include_maybe <- c_index %>%  filter(species %in% opt_list_merged,cyclicity_index>=0.5,func_group=="Dinoflagellate") %>% 
  select(species) %>% unique()
dino_list_include <- c_index %>%  filter(species %in% opt_list_merged,cyclicity_index>=0.5,func_group=="Dinoflagellate") %>% 
  select(species) %>% unique()
dino_list$species

week_means %>% 
  select(diatoms_list_include$species,"week") %>%
  gather(key="species",value="conc",-c(week)) %>%
  ggplot() + 
  geom_line(aes(x=week,y=conc,color=species)) +
  geom_point(aes(x=week,y=conc,color=species,shape=species)) +
  geom_smooth(aes(x=week,y=conc),method = "",color="black",label="Loess Mean")+
  labs(color = "Species",shape="Species")+
  scale_shape_manual(values=1:length(diatoms_list$species)) +
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

ggsave(filename=paste0(basepath,"/figures/cyclic_index/mean_diatom_group_high_cyclicity_index_include.png"),
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


ggsave(filename=paste0(basepath,"/figures/cyclic_index/mean_dino_group_high_cyclicity_index.png"),
       width=1400,height=800,units="px",dpi=200)
