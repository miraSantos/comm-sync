#PURPOSE: pool batches and compute r.2 and significance testing
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso",
                      "rlang")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-08-23_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/filled/2024-09-11_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))


#depends on pool_batches

head(df_mean_a)


#bar plot of r2 values
mean.r2 %>% filter(taxa %in% label_maybe_include) %>%
  ggplot() + geom_bar(aes(x=reorder(taxa,+mean),y=mean),stat="identity")+
  coord_flip() + labs(x="Taxa",y=expression("r"^2)) 
  
  
  #isolate functional groups values
func.r.2 <- shift.res.all %>%
    mutate(taxa = case_when(taxa == "Diatom_noDetritus"~"Diatom",
                            taxa == "NanoFlagCocco"~"Misc. Nanoplankton",
                            .default = taxa)) %>%
    filter(taxa %in% func_group_list) %>% 
    group_by(taxa) %>% summarize(mean = mean(r.2),sd = sd(r.2)) %>% 
    rename(func_group= taxa)
  
  str(func.r.2)
  
  #histogram of R^2 values (individual)
  shift.res.all %>% 
    filter(func_group %in% func_group_list) %>%
    group_by(taxa) %>% 
    mutate(mean = mean(r.2),sd=sd(r.2))%>%
    ggplot() +  geom_histogram(aes(x=mean,
                                   y=after_stat(count/ave(count, PANEL, FUN = sum))),
                               color="white",fill="#009E73",bins=12,alpha=0.5)+
    facet_grid(rows=vars(as.factor(func_group)))+
    geom_vline(data =func.r.2,
               aes(xintercept=mean),color="#E69F00",linewidth=1.5) + 
    xlim(0,1)+  
    facet_grid(rows=vars(as.factor(func_group)))+
    labs(color  = "Functional\nGroup", fill = "Functional\nGroup",
         x = expression("R"^2), y ="Percent (%)")+
    theme_bw()+
    scale_y_continuous(labels = scales::percent)+
    theme(axis.text.x = element_text(angle = -45,
                                     vjust = 0.1,
                                     hjust=0.2))
  
  ggsave(filename=paste0(basepath,"figures/histogram_R2_",Sys.Date(),".png"),
         width=1000, height=2000,units="px",dpi = 300)
  
  
  
  ################################################################################
  # bar plot of R2
  ################################################################################
  #for colorcoding text by functional group
  my_colors <- RColorBrewer::brewer.pal(7, "Dark2")
  map <- data.frame(func_group=func_group_list,colors=my_colors)
  color_code = left_join(mean.r2[order(mean.r2$mean,mean.r2$func_group),],map,
                         by="func_group",relationship = "many-to-many")$colors
  map_dict <- map$colors
  names(map_dict) <- map$func_group
  
  #bar plot of r^2 values
  mean.r2 %>% filter(taxa %in% label_maybe_include) %>%
    ggplot() + geom_bar(aes(x=reorder(taxa,+mean),y=mean,fill=func_group),stat="identity")+
    coord_flip() + 
    labs(x = "Taxa",y = expression("R"^2))+
    scale_fill_manual(values=map_dict,name="Functional\nGroup") + 
    theme(
      panel.background = element_rect(fill = "white", colour = "black",
                                      linewidth = 0.75, linetype = "solid"),
      panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                       colour = "gray"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))
  
  ggsave(filename=paste0(basepath,"figures/R2/barplot_R_all_",Sys.Date(),".png"),
         width = 2200,height=3300,units="px",dpi = 300)
  
  
  #bar plot of r^2 values HORIZONTAL
  mean.r2 %>% filter(taxa %in% label_maybe_include) %>%
    ggplot() + geom_bar(aes(x=reorder(taxa,-mean),y=mean,fill=func_group),stat="identity")+
    labs(x = "Taxa",y = expression("R"^2))+
    scale_fill_manual(values=map_dict,name="Functional\nGroup") + 
    theme(
      panel.background = element_rect(fill = "white", colour = "black",
                                      linewidth = 0.75, linetype = "solid"),
      panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                       colour = "gray"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
      plot.margin=unit(c(0,0,0,1),units="cm"))
    
  
  ggsave(filename=paste0(basepath,"figures/R2/barplot_R_all_horizontal_",Sys.Date(),".png"),
         width = 4300,height=2000,units="px",dpi = 300)
  
  ################################################################################
  # bar plot of Amplitude Variance
  ################################################################################
  #for colorcoding text by functional group
  my_colors <- RColorBrewer::brewer.pal(4, "Dark2")
  map <- data.frame(func_group=func_group_list,colors=my_colors)
  color_code = left_join(shift.res.all[order(shift.res.all$amp_var,shift.res.all$func_group),],map,
                         by="func_group",relationship = "many-to-many")$colors
  map_dict <- map$colors
  names(map_dict) <- map$func_group
  
  shift.res.all %>% filter(taxa %in% label_maybe_include) %>% 
    group_by(taxa) %>%
    summarise(var = mean(amp_var),func_group=first(func_group)) %>%
    ggplot() + geom_bar(aes(x=reorder(taxa,+var),y=var,fill=func_group),stat="identity")+
    # geom_errorbar(aes(x=reorder(taxa,+mean),y=mean,ymin=mean-sd,ymax=mean+sd),stat="identity")+
    scale_fill_manual(values=map_dict,name="Functional\nGroup") + 
    coord_flip() + 
    labs(x = "Taxa",y = "Variance in Amplitude") + 
    scale_fill_manual(values=map_dict,name="Functional\nGroup") + 
    theme(
      panel.background = element_rect(fill = "white", colour = "black",
                                      linewidth = 0.75, linetype = "solid"),
      panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                       colour = "gray"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
  ggsave(filename=paste0(basepath,"figures/R2/barplot_amp_var_all_",Sys.Date(),".png"),
         width = 2000,height=4300,units="px",dpi = 300)
  
  shift.res.all %>% filter(taxa %in% label_maybe_include) %>% 
    group_by(taxa) %>%
    summarise(var = mean(amp_var),func_group=first(func_group)) %>%
    ggplot() + geom_bar(aes(x=reorder(taxa,+var),y=var,fill=func_group),stat="identity")+
    # geom_errorbar(aes(x=reorder(taxa,+mean),y=mean,ymin=mean-sd,ymax=mean+sd),stat="identity")+
    scale_fill_manual(values=map_dict,name="Functional\nGroup") + 
    coord_flip() + 
    labs(x = "Taxa",y = "Variance in Amplitude") + 
    scale_fill_manual(values=map_dict,name="Functional\nGroup") + 
    theme(
      panel.background = element_rect(fill = "white", colour = "black",
                                      linewidth = 0.75, linetype = "solid"),
      panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                       colour = "gray"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
  shift.res.all %>% filter(taxa %in% label_maybe_include) %>% 
    group_by(taxa) %>%
    summarise(mean = mean(amp_var),sd=sd(amp_var),func_group=first(func_group)) %>%
    ggplot() + geom_bar(aes(x=reorder(taxa,-mean),y=mean,fill=func_group),stat="identity")+
    # geom_errorbar(aes(x=reorder(taxa,+mean),y=mean,ymin=mean-sd,ymax=mean+sd),stat="identity")+
    scale_fill_manual(values=map_dict,name="Functional\nGroup") + 
    labs(x = "Taxa",y = "Mean Variance in Amplitude") + 
    scale_fill_manual(values=map_dict,name="Functional\nGroup") + 
    theme(
      panel.background = element_rect(fill = "white", colour = "black",
                                      linewidth = 0.75, linetype = "solid"),
      panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                       colour = "gray"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
      plot.margin=unit(c(0,0,0,1.5),units="cm"))
  
  
  ggsave(filename=paste0(basepath,"figures/R2/barplot_amp_var_all_horizontal_",Sys.Date(),".png"),
         height = 2000,width=4300,units="px",dpi = 300)
  
  

  head(shift.res.all)
  
##################################################################3
#seasonal variation 
################################################################3
  
names(shift.res.all)
shift.long <- shift.res.all %>% pivot_longer(
                                all_of(c("amp_var","seasonal_var","rss")), 
                                names_to = "type",
                                values_to="statistic")
names(shift.long) 


shift.long %>%
  filter(taxa %in% label_maybe_include)%>% 
  ggplot(aes(fill=type, y=statistic,
                x=reorder(taxa,-statistic))) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  scale_y_log10() + 
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95))
  
  ################
  #comparison
  ###################
  load(paste0(basepath,"data/r_objects/c_index_df_cor_2024-06-27.RData"))
  
  
head(mean.r2)
head(c_index)
index_merged <- full_join(mean.r2,c_index,by=c("taxa","func_group"))
str(index_merged)

index_merged %>% ggplot() + geom_point(aes(x =max_xcorr,
                                           y = mean)) +
  geom_abline(aes(slope=1,intercept = 0))+xlim(0,1)+ylim(0,1)+
  labs(x="C Index",y=expression("R"^2))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95))




super_a[["Ditylum_brightwellii"]] %>% ggplot() + geom_point(aes_string(x ="syear",y="Ditylum_brightwellii")) + 
  geom_point(aes(x = syear, y = seasonal_mean),color="red")+
  scale_y_log10()
