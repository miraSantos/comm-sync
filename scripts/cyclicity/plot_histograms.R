
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","car","graphics","stats")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-06-27_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-27_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/c_index_df_cor_2024-06-27.RData"))
load(paste0(basepath,"/data/r_objects/c_index_df_group_2024-06-27.RData"))
################################################################################
#### HISTOGRAM of cyclity
###############################################################################
bin_count = 12
set.seed(7)

func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton","Metazoan","Cyanobacteria","Picoeukaryotes")

my_colors <- RColorBrewer::brewer.pal(7, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
map_dict <- map$colors
names(map_dict) <- map$func_group

######################################################################
#plot histogram
#####################################################################

#plot individual interpolation
y = 2023
test <- ref_year_interp %>% ungroup() %>% filter(year == y) %>% select(protist_tricho_labelC)
df_carbonC_wyear_mean %>% filter(year==y) %>% ggplot() +
  geom_point(aes_string(x=week,y=))+
  geom_point(data=ref_year_interp[ref_year_interp$year==y,],aes(x=week,y=Acantharia,color="red"),shape=3)+
  geom_point(data=test,aes(x=week,Acantharia),shape=3,color="blue")


#introduce cyclic index of groups 
c_index_group_cut <- c_index_group %>%
                      filter(func_group %in% c("Diatom_noDetritus",
                                        "Dinoflagellate",
                                        "Ciliate",
                                        "NanoFlagCocco")) %>%
                     mutate(func_group=c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton")) 
rownames(c_index_group_cut) <- c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton")                  

#four groups in a facet grid as columns
c_index %>% 
  filter(taxa %in% label_maybe_include,
         func_group %in% c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton"))%>%
  ggplot() +
  geom_histogram(aes(x=max_xcorr,
                     y=after_stat(count/ave(count, PANEL, FUN = sum))),
                 color="white",bins=15,alpha=0.5)+
  geom_vline(data=c_index_group_cut,
             aes(xintercept=max_xcorr),color="red")+
  xlim(0,1)+  
  facet_grid(cols=vars(as.factor(func_group)))+
  labs(color  = "Functional\nGroup", fill = "Functional\nGroup",
       x = "Lag-Adjusted Cyclicity Index", y ="Percent (%)")+
  theme_bw()+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = -45, vjust = 0.1, hjust=0.2))

ggsave(filename=paste0(basepath,
                       "/figures/cyclic_index/histogram_cyclic_index_mean_grid_percent_horizontal_xcorr_",Sys.Date(),".png"),
       width=2300,height=700,units="px",dpi=300)



c_index %>% 
  filter(taxa %in% label_maybe_include,
         func_group %in% c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton"))%>%
  ggplot() +
  geom_histogram(aes(x=cyclicity_index,
                     y=after_stat(count/ave(count, PANEL, FUN = sum))),
                 color="white",bins=15,alpha=0.5)+
  geom_vline(data=c_index_group_cut,
             aes(xintercept=cyclicity_index),color="red")+
  xlim(-0.1,1)+  
  facet_grid(rows=vars(as.factor(func_group)))+
  labs(color  = "Functional\nGroup", fill = "Functional\nGroup",
       x = "Cyclicity Index", y ="Percent (%)")+
  theme_bw()+
  scale_y_continuous(labels = scales::percent)

ggsave(filename=paste0(basepath,
                       "/figures/cyclic_index/histogram_cyclic_index_mean_grid_percent_vertical_",Sys.Date(),".png"),
       width=800,height=1700,units="px",dpi=300)

c_index %>% 
  filter(taxa %in% label_maybe_include,
         func_group %in% c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton"))%>%
  ggplot() +
  geom_histogram(aes(x=cyclicity_index,
                     y=after_stat(count/ave(count, PANEL, FUN = sum))),
                 color="white",bins=15,alpha=0.5)+
  geom_vline(data=c_index_group_cut,
             aes(xintercept=cyclicity_index),color="red")+
  xlim(-0.1,1)+  
  facet_wrap(vars(as.factor(func_group)))+
  labs(color  = "Functional\nGroup", fill = "Functional\nGroup",
       x = "Cyclicity Index", y ="Percent (%)")+
  theme_bw()+
  scale_y_continuous(labels = scales::percent)

ggsave(filename=paste0(basepath,
                       "/figures/cyclic_index/histogram_cyclic_index_mean_grid_percent_",Sys.Date(),".png"),
       width=1200,height=1000,units="px",dpi=300)

################################################################################
#PLOT INDIVIDUAL FUNCTIONAL GROUPS
################################################################################


c_index  %>% filter(taxa %in% label_maybe_include)%>%
  ggplot() + 
  geom_histogram(aes(x=cyclicity_index,y = after_stat(count/sum(count))),bins=10) +
  scale_y_continuous(labels = scales::percent)+
  xlab("Cyclicity Index (Median Correlation)") + ylab("Percent") +
  theme_bw()

c_index  %>% filter(taxa %in% label_maybe_include,func_group %in% c("Ciliate")) %>% ggplot() +
  geom_histogram(aes(cyclicity_index,fill=func_group,color=func_group,linetype=func_group),bins=15,alpha=0.5)+
  xlim(0,1)+  
  xlab("Cyclicity Index (Median Correlation)") + ylab("Count") + 
  scale_fill_manual(values=map_dict)+
  scale_color_manual(values=map_dict,name="Functional\nGroup")+
  scale_linetype_manual(values=c("twodash","solid","longdash","dotted","dashed","dotdash","dashed","dotdash"))+
  labs(color  = "Functional\nGroup", linetype ="Functional\nGroup", fill = "Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white")
  )

c_index  %>% filter(taxa %in% label_maybe_include,func_group=="Misc. Nanoplankton") %>% ggplot() +
  geom_histogram(aes(cyclicity_index,fill=func_group,color=func_group,linetype=func_group),bins=10,alpha=0.5)+
  xlim(0,1)+  
  xlab("Cyclicity Index (Median Correlation)") + ylab("Count") + 
  scale_fill_manual(values=map_dict)+
  scale_color_manual(values=map_dict,name="Functional\nGroup")+
  scale_linetype_manual(values=c("twodash","solid","longdash","dotted","dashed","dotdash","dashed","dotdash"))+
  labs(color  = "Functional\nGroup", linetype ="Functional\nGroup", fill = "Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white")
  )


############################3
#LAG
names(df_cor)
head(df_cor)
