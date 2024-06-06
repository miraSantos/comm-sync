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

####################################################
#PLOT individual correlation over time
###############################################3
ii = which(protist_tricho_labelC=="Acantharia")
ggplot(data=df_cor) + geom_point(aes_string(x="year",y=protist_tricho_labelC[ii]))+
  geom_segment(aes_string(x="year",y=0,xend="year",yend=protist_tricho_labelC[ii]))+
  geom_hline(aes(yintercept=0),color="black")+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
  ylab("Correlation Coefficient") +xlab("Year")+
  ggtitle(protist_tricho_labelC[ii])

ggplot(df_carbonC_wyear_mean) + geom_point(aes(x=date,y=detritus))

ggplot(data=df_cor) + geom_point(aes_string(x="year",y=protist_tricho_labelC[ii]))+
  geom_line(aes_string(x="year",y=protist_tricho_labelC[ii]))+
  geom_hline(aes(yintercept=0),color="black")+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
  ylab("Correlation Coefficient") +xlab("Year")+
  ggtitle(protist_tricho_labelC[ii])

# df_cor %>% gather(key="class",value="corr_coef",-c("year")) %>% 
#   group_by(class) %>%
#   %>%
#   ggplot() + geom_boxplot(aes(x=reorder(func_group,y=corr_coef))+coord_flip()+
#   ylim(0,1)+
#   labs(x="Taxa",y="Cyclicity Index")
# 

###################################################################################
#plot c_index_merged
##################################################################################
func_group_list = c("Diatom","Dinoflagellate","Ciliate","Nano-Flag-Cocco","Metazoan","Other")

#for colorcoding text by functional group
my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(c_index_merged[order(c_index_merged$cyclicity_index,c_index_merged$func_group),],map,by="func_group")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group
label_maybe_include = append(label_maybe_include,c("Picoeuks","Synechococcus"))
label_include = append(label_include,c("Picoeuks","Synechococcus"))

bar_c_index_merged_include <- c_index_merged %>% filter(species %in% label_include)%>%
  ggplot() + geom_bar(aes(x=reorder(species,+cyclicity_index),
                          y=cyclicity_index,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(species,+cyclicity_index),
                    y=cyclicity_index,ymin=cyclicity_index-sd,
                    ymax=cyclicity_index+sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  coord_flip()+
  ylab("Cyclicity Index")+xlab("Species")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )
bar_c_index_merged_include_maybe <- c_index_merged %>% filter(species %in% label_maybe_include)%>%
  ggplot() + geom_bar(aes(x=reorder(species,+cyclicity_index),
                          y=cyclicity_index,
                          fill=func_group),
                      stat="identity")+
  geom_errorbar(aes(x=reorder(species,+cyclicity_index),
                    y=cyclicity_index,ymin=cyclicity_index-sd,
                    ymax=cyclicity_index+sd),
                color="black", width=.01) +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Cyclicity Index")+xlab("Species")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )+coord_flip()
bar_c_index_merged_include_maybe


bar_c_index_merged_include
ggsave(filename=paste0(basepath,"/figures/cyclic_index_merged/cyclic_index_merged_quadroot_median_include_error_bar_",Sys.Date(),".png"),
       width=1500,height=2000,units="px",dpi=200)

bar_c_index_merged_include_maybe

ggsave(filename=paste0(basepath,"/figures/cyclic_index_merged/cyclic_index_merged_quadroot_median_include_maybe_error_bar_",Sys.Date(),".png"),
       width=1500,height=3000,units="px",dpi=200)

#################################################################################
# Standard Deviation Bar Plot
#################################################################################
ggplot(data=c_index_merged) + geom_bar(aes(x=reorder(species,+sd),
                                    y=sd,
                                    fill=func_group),
                                stat="identity")+
  coord_flip()+
  ylab("Standard Deviation")+xlab("Species")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")

ggsave(filename=paste0(basepath,"/figures/cyclic_index_merged/cyclic_index_merged_sd.png"),
       width=1500,height=3000,units="px",dpi=200)

############################
#DTW
#############################

my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(c_index_merged[order(c_index_merged$dtw,c_index_merged$func_group),],map,by="func_group")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group

c_index_merged %>% ggplot() +
  geom_bar(aes(x=reorder(species,+dtw),y=dtw,fill=as.factor(func_group)),
           stat="identity")+
  coord_flip()+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  xlab("dtw Distance") + ylab("Species")+
  theme(axis.text.y = element_text(colour = color_code))


ggsave(filename=paste0(basepath,"/figures/cyclic_index_merged/cyclic_index_merged_quadroot_median_dtw.png"),
       width=1500,height=3000,units="px",dpi=200)

ggplot(data=df_carbonC)+geom_point(aes(x=date,y=Paralia_sulcata))+
  scale_x_date()
