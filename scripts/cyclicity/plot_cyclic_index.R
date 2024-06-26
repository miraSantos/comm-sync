basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-12_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-12_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"data/r_objects/c_index_df_cor_2024-06-13.RData"))

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


bar_c_index_include <- c_index %>% filter(species %in% label_include)%>%
  ggplot() + geom_bar(aes(x=reorder(species,+max_xcorr),
                          y=max_xcorr,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(species,+max_xcorr),
                    y=cyclicity_index,ymin=max_xcorr-max_xcorr_sd,
                    ymax=max_xcorr+max_xcorr_sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  coord_flip()+
  ylab("Lag-Adjusted Cyclicity Index")+xlab("Taxa")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )
bar_c_index_include_maybe <- c_index %>% filter(species %in% label_maybe_include)%>%
  ggplot() + geom_bar(aes(x=reorder(species,+cyclicity_index),
                          y=cyclicity_index,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(species,+cyclicity_index),
                    y=cyclicity_index,ymin=cyclicity_index-sd,
                    ymax=cyclicity_index+sd),
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

ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_quadroot_median_include_maybe_error_bar_",Sys.Date(),".png"),
       width=1500,height=3000,units="px",dpi=200)

#

bar_c_index_include_maybe_split <- c_index %>% filter(species %in% label_maybe_include)%>%
  ggplot() + geom_bar(aes(x=reorder(species,+cyclicity_index),
                          y=cyclicity_index,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(species,+cyclicity_index),
                    y=cyclicity_index,ymin=cyclicity_index-sd,
                    ymax=cyclicity_index+sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Cyclicity Index")+xlab("Taxa")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
bar_c_index_include_maybe_split

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
