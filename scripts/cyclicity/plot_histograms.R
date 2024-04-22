
################################################################################
#### HISTOGRAM of cyclity
###############################################################################
bin_count = 12
set.seed(7)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colors = sample(cbbPalette,4)

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
  xlim(0,1)+ ylim(0,0.006)+ 
  xlab("Cyclicity Index (Median Correlation)") + ylab("Normalized Density") + 
  guides(fill=guide_legend(title="Functional Group"))+
  scale_color_manual(name="Functional Group",
                     labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco"),
                     values = colors)+
  scale_fill_manual(name="Functional Group",
                    labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco"),
                    values = colors)+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

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
  xlab("Standard Deviation of Cyclicity Index") + ylab("Normalized Density") + 
  guides(fill=guide_legend(title="Functional Group"))+
  scale_color_manual(name="Functional Group",
                     labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco"),
                     values = colors)+
  scale_fill_manual(name="Functional Group",
                    labels=c("Ciliate","Diatom","Dinoflagellate","Nano-Flag-Cocco"),
                    values = colors)+
  xlim(0,0.5)+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )


ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_standard_deviation_quadroot.png"),
       width=1500,height=800,units="px",dpi=200)


combined <- hist_c_index + hist_sd & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A')

ggsave(filename=paste0(basepath,"/figures/cyclic_index/histogram_grid.png"),
       width=2550,height=1000,units="px",dpi=300)

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

