basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","ggpubr")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-12_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-12_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"data/r_objects/c_index_df_cor_2024-06-27.RData"))

####################################################
#PLOT individual correlation over time
###############################################3
ii = which(protist_tricho_labelC=="Corethron_hystrix")

#plot correlation coefficient over time
ggplot(data=df_cor) + geom_point(aes_string(x="year",y=protist_tricho_labelC[ii]))+
  geom_line(aes_string(x="year",y=protist_tricho_labelC[ii]))+
  geom_hline(aes(yintercept=0),color="black")+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
  ylab("Correlation Coefficient") +xlab("Year")+
  ggtitle(protist_tricho_labelC[ii])

df_carbonC_wyear_mean %>% mutate_at(protist_tricho_labelC,quadroot) %>% 
  ggplot() + geom_point(aes_string(x="date",y=protist_tricho_labelC[ii]),size=0.5)+
  scale_x_date(date_breaks="1 year",date_labels=format("%Y"))


index_ranking <- order(c_index$cyclicity_index,c_index$func_group)
c_index$split_facet <- 1
c_index$split_facet[index_ranking[1:40]] <- 3
c_index$split_facet[index_ranking[41:80]] <- 2
c_index$split_facet[index_ranking[81:118]] <- 1


###################################################################################
#plot c_index
##################################################################################

#for colorcoding text by functional group
my_colors <- RColorBrewer::brewer.pal(7, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(c_index[order(c_index$max_xcorr,c_index$func_group),],map,by="func_group",relationship = "many-to-many")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group


bar_c_index_include <- c_index %>% filter(taxa %in% label_include)%>%
  ggplot() + geom_bar(aes(x=reorder(taxa,+max_xcorr),
                          y=max_xcorr,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(taxa,+max_xcorr),
                    y=max_xcorr,ymin=max_xcorr-max_xcorr_sd,
                    ymax=max_xcorr+max_xcorr_sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  coord_flip()+
  ylab("Lag-Adjusted Cyclicity Index")+xlab("Taxa")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

c_index<- left_join(c_index,mb,by="taxa")
bar_c_index_include_maybe <- c_index %>% filter(taxa %in% c(label_maybe_include))%>%
  ggplot() + geom_bar(aes(x=reorder(taxa,+mean_conc),
                          y=max_xcorr,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(taxa,+max_xcorr),
                    y=max_xcorr,ymin=max_xcorr-max_xcorr_sd,
                    ymax=max_xcorr+max_xcorr_sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Lag-Adjusted Cyclicity Index")+xlab("Taxa")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )+coord_flip()


bar_c_index_include

ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_quadroot_median_include_lag_adjusted_error_bar_",Sys.Date(),".png"),
       width=1500,height=2000,units="px",dpi=200)

bar_c_index_include_maybe







ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_quadroot_median_include_maybe_lag_adjusted_error_bar_SORTED_BIOMASS",Sys.Date(),".png"),
       width=1500,height=3000,units="px",dpi=200)



bar_c_index_include_maybe_horizontal <- c_index %>% filter(taxa %in% c(label_maybe_include))%>%
  ggplot() + geom_bar(aes(x=reorder(taxa,+mean_conc),
                          y=max_xcorr,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(taxa,+max_xcorr),
                    y=max_xcorr,ymin=max_xcorr-max_xcorr_sd,
                    ymax=max_xcorr+max_xcorr_sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Lag-Adjusted Cyclicity Index")+xlab("Taxa")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))
  
bar_c_index_include_maybe_horizontal <- c_index %>% filter(taxa %in% c(label_maybe_include))%>%
  ggplot() + geom_bar(aes(x=reorder(taxa,-mean_conc),
                          y=max_xcorr,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(taxa,+max_xcorr),
                    y=max_xcorr,ymin=max_xcorr-max_xcorr_sd,
                    ymax=max_xcorr+max_xcorr_sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Lag-Adjusted Cyclicity Index")+xlab("Taxa")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
    plot.margin=unit(c(0,0,0,1),units="cm"))
  
#top,right,bottom,left
bar_c_index_include_maybe_horizontal


ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_quadroot_median_include_maybe_lag_adjusted_error_bar_horiztonal_SORTED_",Sys.Date(),".png"),
       width=4300,height=2000,units="px",dpi=300)


########################3
# combined
#########################
bar_c_index_include_maybe_half <- c_index %>% filter(taxa %in% c(label_maybe_include))%>%
  ggplot() + geom_bar(aes(x=reorder(taxa,+max_xcorr),
                          y=max_xcorr,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(taxa,+max_xcorr),
                    y=max_xcorr,ymin=max_xcorr-max_xcorr_sd,
                    ymax=max_xcorr+max_xcorr_sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Lag-Adjusted Cyclicity Index")+xlab("Taxa")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    legend.text = element_text(size=10))+
  coord_flip()
bar_c_index_include_maybe_half

lag_heatmap <- df_merge_lag_max %>% filter(taxa %in% label_maybe_include) %>%
  ggplot() +
  geom_tile(aes(x=year,y=reorder(taxa,+laci),fill=lag))+
  scale_fill_gradient2(midpoint = 0, mid="#eee8d5", high="#dc322f", low="#268bd2")+
  theme_bw()+
  theme(axis.text.y = element_blank(),axis.title.y=element_blank())+
  labs(x="Year",y="Taxa",fill="Lag")+
  scale_x_continuous(breaks=seq(2006,2022,2))

lag_heatmap

ggarrange(bar_c_index_include_maybe_half,lag_heatmap,labels=c("A","B"),
          legend="top",ncol=2,nrow=1,
          widths=c(1,0.5))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_quadroot_median_include_maybe_lag_adjusted_error_bar_combined_",Sys.Date(),".png"),
        width=3000,height=3800,units="px",dpi=300)


#################################################################################
# Standard Deviation Bar Plot
#################################################################################
ggplot(data=c_index) + geom_bar(aes(x=reorder(species,+sd),
                                    y=sd,
                                    fill=func_group),
                                stat="identity")+
  coord_flip()+
  ylab("Standard Deviation")+xlab("Taxa")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")

ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_sd.png"),
       width=1500,height=3000,units="px",dpi=200)
