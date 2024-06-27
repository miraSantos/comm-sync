#PURPOSE: extract contours from wavelet plot and measure x and y dimension
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices",
                      "rlist","rapport","forcats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source(paste0(basepath,"/scripts/wavelet/adv_biwavelet_packages.R"))
source(paste0(basepath,"/scripts/wavelet/plot_single_wt_arc.R"))

################################################################################
################################################################################
#COMPUTE CONTOUR LINES FOR ANNUAL PERIOD
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

load(paste0(basepath,"data/r_objects/unfilled/2024-06-12_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-12_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))

load(paste0(basepath,"data/r_objects/2024-06-06_df_carbonC_filled_merged_super_res_morlet.RData"))
#store dimensions of annual contours
annual_lengths <- vector(length=length(super_res))
annual_dims = data.frame(species=character(),xmin=numeric(),xmax=numeric())
Colnames <- names(annual_dims)


for(wavelet in 1:length(super_res)){
  print(wavelet)
#extract dimensions of contours
  x = super_res[[wavelet]]
  species = protist_tricho_label_merged[wavelet]
  yvals <- log2(super_res[[wavelet]]$period)
  tol = 1
  contour_pos <- contourLines(x$t, yvals, t(x$signif), level = tol)

  # extract contours at annual band
  annual_contours <- list()
  annual_contours_length <- vector(length=length(contour_pos))
  #annual band period range
  day_x0 = 300
  day_x = 500
  period_min = log2(day_x0) 
  period_max = log2(day_x) 
  for(ii in 1:length(contour_pos)){
    annual_contours[[ii]] <- which((contour_pos[[ii]]$y > period_min) & (contour_pos[[ii]]$y < period_max))
    annual_contours_length[ii] <- length(annual_contours[[ii]])
  }

  #compute length of x and y dimensions for each annual band
  ranges = list()
  range_i = list()
  wavelet_annual_length_sum = 0
  annual_index = which(annual_contours_length !=0)

  #loop through each polygon with annual if present
  if(length(annual_index > 0)){
    for(annual_i in 1:length(annual_index)){
      #find range witihn countour
      range_i$y = range(contour_pos[[annual_index[annual_i]]]$y)
      range_i$x = range(contour_pos[[annual_index[annual_i]]]$x)
      xmin = range_i$x[1]
      xmax = range_i$x[2]
      if((xmin > (365*14))|xmax<(365*2)){break}
      #removing ranges to account for cone of influence
      xmin = if_else(xmin <= 365*2,1,xmin-(365*2))
      xmax = pmin(xmax,365*14)
      length_x = xmax-xmin
      wavelet_annual_length_sum = wavelet_annual_length_sum + length_x
      annual_dims <- rbind(annual_dims, c(species,xmin,xmax))
      names(annual_dims) <- Colnames
    }
  }
  wavelet_annual_length_sum = pmin(wavelet_annual_length_sum,365*14-1)
  annual_lengths[wavelet] <-wavelet_annual_length_sum
}

annual_dims <- annual_dims %>% mutate(xmin=as.numeric(xmin),xmax=as.numeric(xmax),func_group = "Other")
#store annual durations in dataframe with species and functional group labels
df_annual = data.frame(annual_duration = annual_lengths,species=protist_tricho_label_merged,func_group = "Other")
#create column with functional group 


#load ifcb class list file that categories each species in a functional group
func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton","Metazoan","Synechococcus","Picoeukaryotes")
func_group_labels <- list(diatom_labelC,dino_label,ciliate_label,nfg_label,metazoan_label,c("Synechococcus"),c("Pico_eukaryotes"))
#create column with functional group for df annual
for(func_group in 1:length(func_group_list)){
reference=func_group_labels[[func_group]]
df_annual[df_annual$species%in%reference,"func_group"] = func_group_list[func_group]
annual_dims[annual_dims$species%in%reference,"func_group"] = func_group_list[func_group]
}
annual_dims <- left_join(annual_dims,df_annual,by=c("species","func_group"))


full_periodicity_list <- df_annual$species[which(df_annual$annual_duration==5109)]
full_periodicity_list
save(df_annual,annual_dims,full_periodicity_list,
     file=paste0(basepath,"/data/r_objects/df_annual_periodicity_contour",Sys.Date(),".RData"))
################################################################################
#bar chart
################################################################################
df_annual %>%group_by(func_group) %>%
  summarise(ad_sum = sum(annual_duration),n=n()) %>% 
  ggplot() +
  geom_bar(aes(x=func_group,y=ad_sum/n),stat="identity")+
  xlab("Functional Group") + 
  ylab("Days with Annual Periodicity")


####################################################################################
#complete bar plot 
####################################################################################
#generating color codes
my_colors <- RColorBrewer::brewer.pal(7, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(df_annual[order(df_annual$annual_duration,df_annual$func_group),],map,by="func_group")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group

df_annual[order(df_annual$annual_duration,df_annual$func_group),] %>%
  filter(species %in% label_maybe_include)%>%
  mutate(species=factor(species,levels=species)) %>%
  ggplot() +
  geom_bar(aes(x= species,y=annual_duration,fill=func_group),
           stat="identity",color="white")+
  coord_flip()+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Years with Annual Periodicity")+
  xlab("Species")+
  scale_y_continuous(breaks=seq(0,365*14,365*2),
                     labels=seq(0,14,2),limits=c(0,365*14))+
  theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

ggsave(filename=paste0(basepath,"figures/wavelet_transforms/annual_periodicity_bar_all_",Sys.Date(),".png"),
       width=1500,height=3000,units="px",dpi=200)

df_annual[order(df_annual$annual_duration,df_annual$func_group,decreasing=T),] %>%
  filter(species %in% label_maybe_include)%>%
  mutate(species=factor(species,levels=species)) %>%
  ggplot() +
  geom_bar(aes(x= species,y=annual_duration,fill=func_group),
           stat="identity")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Years with Annual Periodicity")+
  xlab("Taxa")+
  scale_y_continuous(breaks=seq(0,365*14,365*2),
                     labels=seq(0,14,2),limits=c(0,365*14))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    plot.margin = unit(c(0, 0.1, 0, 0.4), 
                       "inches"),
    legend.position = c(1, 1),
    legend.title = element_text(size = 6), 
    legend.text = element_text(size = 6))+
  guides(fill = guide_legend(override.aes = list(size = 0.001)))

          

ggsave(filename=paste0(basepath,"figures/wavelet_transforms/annual_periodicity_bar_horizontal_all_",Sys.Date(),".png"),
       width=3700,height=1600,units="px",dpi=300)


head(df_annual)

df_annual_pie <- df_annual %>% filter(species%in% label_maybe_include) %>% mutate(pie = case_when(annual_duration==5109 ~ "Full",
                                     (annual_duration<5109 & annual_duration>0) ~ "Partial",
                                     annual_duration==0 ~ "None"))  %>% 
  group_by(pie) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))


ggplot(df_annual_pie, aes(x = "", y = perc, fill = pie)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+theme_void()+
  guides(fill = guide_legend(title = "Periodicity")) 

ggsave(filename=paste0(basepath,"figures/wavelet_transforms/annual_periodicity_pie_chart",Sys.Date(),".png"),
       width=800,height=600,units="px",dpi=200)
  
################################################################################
# Bar plot only of complete taxa
################################################################################

my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(df_annual[order(df_annual$annual_duration,df_annual$func_group),],map,by="func_group") %>% 
  filter(annual_duration > 365*2) %>% select(colors)
map_dict <- map$colors
names(map_dict) <- map$func_group

df_annual[order(df_annual$annual_duration,df_annual$func_group),] %>% 
  filter(annual_duration > 365*2) %>% 
  mutate(species=factor(species,levels=species)) %>% 
  ggplot() +
  geom_bar(aes(x=species,y=annual_duration,fill=func_group),
           stat="identity",color="white")+
  coord_flip()+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylab("Years with Annual Periodicity")+
  xlab("Species")+
  scale_y_continuous(breaks=seq(0,365*14,365*2),
                     labels=seq(0,14,2),limits=c(0,365*14))+
  theme(axis.text.y = element_text(colour = color_code$colors))

ggsave(filename=paste0(basepath,"figures/wavelet_transforms/annual_periodicity_bar_full_partial",Sys.Date(),".png"),
       width=1600,height=1600,units="px",dpi=200)


df_annual %>% filter(annual_duration <5109)
annual_dims %>% filter(annual_duration <5109)
################################################################################
#ANNUAL TIME RANGE PLOT
################################################################################
annual_dims %>% 
  filter(annual_duration<5050,species %in% label_maybe_include)%>%
  mutate(species=factor(species,levels=unique(species))) %>% 
  ggplot() +
  geom_linerange(aes(x=species,
                     ymin=xmin,ymax=xmax,color=func_group),linewidth=3)+
  coord_flip() + 
  ylab("Year") +
  xlab("Species")+
  scale_color_manual(values=map_dict,name="Functional\nGroup") +
  scale_y_continuous(breaks=seq(0,365*14,365*2),
                     labels=seq(2008,2008+14,2),limits=c(0,365*14))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

ggsave(filename=paste0(basepath,"figures/wavelet_transforms/annual_periodicity_line_range_",Sys.Date(),".png"),
       width=1500,height=1000,units="px",dpi=200)

#################################################################################
#compute contour dimensions for an INDIVIDUAL WAVELET
#################################################################################
head(annual_dims)

wavelet = which(protist_tricho_labelC=="Emiliania_huxleyi")
print(wavelet)
#extract dimensions of contours
x = super_res[[wavelet]]
species = protist_tricho_labelC[wavelet]
species
yvals <- log2(super_res[[wavelet]]$period)
tol = 1
#extract contour lines
contour_pos <- contourLines(x$t, yvals, t(x$signif), level = tol)

# extract contours at annual band
annual_contours <- list()
annual_contours_length <- vector(length=length(contour_pos))
#annual band period range
day_x0 = 360
day_x = 370
period_min = log2(day_x0) 
period_max = log2(day_x) 
for(ii in 1:length(contour_pos)){
  annual_contours[[ii]] <- which((contour_pos[[ii]]$y > period_min) & (contour_pos[[ii]]$y < period_max))
  annual_contours_length[ii] <- length(annual_contours[[ii]])
}

#compute length of x and y dimensions for each annual band
ranges = list()
range_i = list()
wavelet_annual_length_sum = 0
annual_index = which(annual_contours_length !=0)

#loop through each polygon with annual if present
if(length(annual_index > 0)){
  for(annual_i in 1:length(annual_index)){
    #find range witihn countour
    range_i$y = range(contour_pos[[annual_index[annual_i]]]$y)
    range_i$x = range(contour_pos[[annual_index[annual_i]]]$x)
    xmin = range_i$x[1]
    xmax = range_i$x[2]
    if((xmin > (365*14))|xmax<(365*2)){break}
    #truncating ranges to account for cone of influence
    xmin = if_else(xmin <= 365*2,1,xmin-(365*2))
    xmax = pmin(xmax,365*14)
    length_x = xmax-xmin
    wavelet_annual_length_sum = wavelet_annual_length_sum + length_x
    annual_dims <- rbind(annual_dims, c(species,xmin,xmax))
    names(annual_dims) <- Colnames
  }
}
wavelet_annual_length_sum = pmin(wavelet_annual_length_sum,365*14-1)
annual_lengths[wavelet] <-wavelet_annual_length_sum

wavelet
protist_tricho_labelC[wavelet]
plot.biwavelet_adv(super_res[[wavelet]])

