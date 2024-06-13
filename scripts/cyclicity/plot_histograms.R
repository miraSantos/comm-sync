
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","car","graphics","stats")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"/data/r_objects/c_index_df_cor_2024_May_13.RData"))

################################################################################
#### HISTOGRAM of cyclity
###############################################################################
bin_count = 12
set.seed(7)


my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
map_dict <- map$colors
names(map_dict) <- map$func_group


#Unnormalized density plot of cyclicity index
c_index  %>% filter(species %in% label_maybe_include) %>% ggplot() +
  geom_density(aes(cyclicity_index,fill=func_group,color=func_group,linetype=func_group),alpha=0.5)+
  xlim(0,1)+  
  xlab("Cyclicity Index (Median Correlation)") + ylab("Normalized Density") + 
  scale_fill_manual(values=map_dict)+
  scale_color_manual(values=map_dict,name="Functional\nGroup")+
  scale_linetype_manual(values=c("twodash","solid","longdash","dotted","dashed"))+
  labs(color  = "Functional\nGroup", linetype ="Functional\nGroup", fill = "Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_cyclicity_index_median_quadroot_normalized",Sys.Date(),".png"),
       width=1500,height=800,units="px",dpi=200)



#ONLY SPECIES WITH INCLUDE TAG
c_index  %>% filter(species %in% label_include) %>% ggplot() +
  geom_density(aes(cyclicity_index,fill=func_group,color=func_group),alpha=0.5)+
  xlim(0,1)+  
  xlab("Cyclicity Index (Median Correlation)") + ylab("Normalized Density") + 
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  scale_color_manual(values=map_dict,name="Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_cyclicity_index_median_quadroot_normalized_include_only_",Sys.Date(),".png"),
       width=1500,height=800,units="px",dpi=200)



#Unnormalized density plot of SD
c_index  %>% filter(species %in% label_maybe_include) %>% ggplot() +
  geom_density(aes(sd,fill=func_group,color=func_group,linetype=func_group),alpha=0.5)+
  xlim(0,0.55)+  
  xlab("Standard Deviation of Cyclicity Index") + ylab("Normalized Density") + 
  scale_fill_manual(values=map_dict)+
  scale_color_manual(values=map_dict,name="Functional\nGroup")+
  scale_linetype_manual(values=c("twodash","solid","longdash","dotted","dashed"))+
  labs(color  = "Functional\nGroup", linetype ="Functional\nGroup", fill = "Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_SD_median_quadroot_normalized_",Sys.Date(),".png"),
       width=1500,height=800,units="px",dpi=200)


#normalized density
hist_c_index
  

hist_c_index <- ggplot() +
  geom_density(data=c_index[c_index$func_group=="Diatom",],
               aes(x=cyclicity_index,y=after_stat(count)/sum(after_stat(count)),
                   fill="Diatom",color="Diatom"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Dinoflagellate",],
               aes(x=cyclicity_index,y=after_stat(count)/sum(after_stat(count)),
                   fill="Dinoflagellate",color="Dinoflagellate"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Nano-Flag-Cocco",],
               aes(x=cyclicity_index,y=after_stat(count)/sum(after_stat(count)),
                   fill="Nano-Flag-Cocco",color="Nano-Flag-Cocco"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Ciliate",],
               aes(x=cyclicity_index,y=after_stat(count)/sum(after_stat(count)),
                   fill="Ciliate",color="Ciliate"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Other",],
               aes(x=cyclicity_index,y=after_stat(count)/sum(after_stat(count)),
                   fill="Other",color="Other"),alpha=0.4)+
  xlim(0,1)+ ylim(0,0.006)+ 
  xlab("Cyclicity Index (Median Correlation)") + ylab("Normalized Density") + 
  guides(fill=guide_legend(title="Functional Group"))+
  scale_color_manual(name="Functional Group",
                     labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco","Other"),
                     values = map_dict)+
  scale_fill_manual(name="Functional Group",
                    labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco","Other"),
                    values = map_dict)+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white")
  )
hist_c_index

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_cyclicity_index_median_quadroot_normalized.png"),
       width=1500,height=800,units="px",dpi=200)

# IDNIVDUAL HISTOGRAM
c_index %>% 
  group_by(func_group) %>% filter(func_group=="Diatom")%>%
  filter(func_group !="Unknown")%>%
  ggplot() + geom_histogram(aes(x=cyclicity_index,
                                fill=func_group),alpha=0.4,bins=12)+
  xlim(0,1)+
  xlab("Cyclicity Index") + 
  guides(fill=guide_legend(title="Functional Group"))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_cyclic_index_quadroot.png"),
       width=1500,height=3000,units="px",dpi=200)


########################################################################
#HISTOGRAM OF SD
#######################################################################
hist_sd <- ggplot() +
  geom_density(data=c_index[c_index$func_group=="Diatom",],
               aes(x=sd,y=after_stat(count)/sum(after_stat(count)),
                   fill="Diatom",color="Diatom"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Dinoflagellate",],
               aes(x=sd,y=after_stat(count)/sum(after_stat(count)),
                   fill="Dinoflagellate",color="Dinoflagellate"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Nano-Flag-Cocco",],
               aes(x=sd,y=after_stat(count)/sum(after_stat(count)),
                   fill="Nano-Flag-Cocco",color="Nano-Flag-Cocco"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Ciliate",],
               aes(x=sd,y=after_stat(count)/sum(after_stat(count)),
                   fill="Ciliate",color="Ciliate"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Other",],
               aes(x=cyclicity_index,y=after_stat(count)/sum(after_stat(count)),
                   fill="Other",color="Other"),alpha=0.4)+
  xlab("Standard Deviation of Cyclicity Index") + ylab("Normalized Density") + 
  guides(fill=guide_legend(title="Functional Group"))+
  scale_color_manual(name="Functional Group",
                     labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco","Other"),
                     values = map_dict)+
  scale_fill_manual(name="Functional Group",
                    labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco","Other"),
                    values = map_dict)+
  xlim(0,0.5)+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

hist_sd

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_standard_deviation_quadroot.png"),
       width=1500,height=800,units="px",dpi=200)


combined <- hist_c_index + hist_sd & theme(legend.position = "bottom")

combined + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A')


bar_c_index + (hist_c_index/hist_sd) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_grid.png"),
       width=2550,height=1000,units="px",dpi=300)


bar_c_index + (hist_c_index/hist_sd) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')

ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_grid.png"),
       width=2550,height=3000,units="px",dpi=300)

##############################
#DTW HISTOGRAM
######################################################

ggplot() +
  geom_density(data=c_index[c_index$func_group=="Diatom",],
               aes(x=dtw,y=after_stat(count)/sum(after_stat(count)),
                   fill="Diatom",color="Diatom"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Dinoflagellate",],
               aes(x=dtw,y=after_stat(count)/sum(after_stat(count)),
                   fill="Dinoflagellate",color="Dinoflagellate"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Nano-Flag-Cocco",],
               aes(x=dtw,y=after_stat(count)/sum(after_stat(count)),
                   fill="Nano-Flag-Cocco",color="Nano-Flag-Cocco"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Ciliate",],
               aes(x=dtw,y=after_stat(count)/sum(after_stat(count)),
                   fill="Ciliate",color="Ciliate"),alpha=0.4)+
  xlab("DTW Normalized Distance") + ylab("Normalized Density") + 
  guides(fill=guide_legend(title="Functional Group"))+
  scale_color_manual(name="Functional Group",
                     labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco"),
                     values = colors)+
  scale_fill_manual(name="Functional Group",
                    labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco"),
                    values = colors)+
  xlim(0,3)+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_median_dtw_quadroot.png"),
       width=1300,height=800,units="px",dpi=200)


#########################################################################
#Histogram of Consistency in Annual Peak
########################################################################

hist_consistency <-ggplot() +
  geom_density(data=c_index[c_index$func_group=="Diatom",],
               aes(x=consistency,y=after_stat(count)/sum(after_stat(count)),
                   fill="Diatom",color="Diatom"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Dinoflagellate",],
               aes(x=consistency,y=after_stat(count)/sum(after_stat(count)),
                   fill="Dinoflagellate",color="Dinoflagellate"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Nano-Flag-Cocco",],
               aes(x=consistency,y=after_stat(count)/sum(after_stat(count)),
                   fill="Nano-Flag-Cocco",color="Nano-Flag-Cocco"),alpha=0.4)+
  geom_density(data=c_index[c_index$func_group=="Ciliate",],
               aes(x=consistency,y=after_stat(count)/sum(after_stat(count)),
                   fill="Ciliate",color="Ciliate"),alpha=0.4)+
  xlab("Consistency in Annual Peak") + ylab("Normalized Density") + 
  guides(fill=guide_legend(title="Functional Group"))+
  scale_color_manual(name="Functional Group",
                     labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco"),
                     values = colors)+
  scale_fill_manual(name="Functional Group",
                    labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco"),
                    values = colors)+
  xlim(0,1)

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_consistency_annual_peak_quadroot.png"),
       width=1500,height=800,units="px",dpi=200)

#plot individual interpolation
y = 2023
test <- ref_year_interp %>% ungroup() %>% filter(year == y) %>% select(protist_tricho_labelC)
df_carbonC_wyear_mean %>% filter(year==y) %>% ggplot() +
  geom_point(aes_string(x=week,y=))+
  geom_point(data=ref_year_interp[ref_year_interp$year==y,],aes(x=week,y=Acantharia,color="red"),shape=3)+
  geom_point(data=test,aes(x=week,Acantharia),shape=3,color="blue")

