#PURPOSE: extract contours from wavelet plot and measure x and y dimension
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices","rlist","rapport","forcats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

################################################################################
################################################################################
#COMPUTE CONTOUR LINES FOR ANNUAL PERIOD
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
load(paste0(basepath,"/data/r_objects/wavelet_output_species_2024_Mar_22_morlet.RData"))


#store dimensions of annual contours
annual_lengths <- vector(length=length(super_res))
annual_dims = data.frame(species=character(),xmin=numeric(),xmax=numeric())
Colnames <- names(annual_dims)

for(wavelet in 1:length(super_res)){
  print(wavelet)
#extract dimensions of contours
  x = super_res[[wavelet]]
  species = protist_tricho_labelC[wavelet]
  yvals <- log2(super_res[[wavelet]]$period)
  tol = 1
  contour_pos <- contourLines(x$t, yvals, t(x$signif), level = tol)

  # extract contours at annual band
  annual_contours <- list()
  annual_contours_length <- vector(length=length(contour_pos))
  #annual band
  period_min = log2(100) #300 days
  period_max = log2(500) #
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

annual_dims <- annual_dims %>% mutate(xmin=as.numeric(xmin),xmax=as.numeric(xmax),func_group = "Unknown")
#store annual durations in dataframe with species and functional group labels
df_annual = data.frame(annual_duration = annual_lengths,species=protist_tricho_labelC,func_group = "Unknown")

annual_dims <- left_join(annual_dims,df_annual,by=c("species","func_group"))

#load ifcb class list file that categories each species in a functional group
func_group_list = c("Diatom","Dinoflagellate","Ciliate","Nano-Flag-Cocco","Metazoan","Unknown")
func_group_labels <- list(diatom_labelC,dino_label,ciliate_label,nanoflagcocco_label,metazoan_label,c("Unknown"))
#create column with functional group for df annual
for(func_group in 1:length(func_group_list)){
reference=func_group_labels[[func_group]]
df_annual[df_annual$species%in%reference,"func_group"] = func_group_list[func_group]
annual_dims[annual_dims$species%in%reference,"func_group"] = func_group_list[func_group]
}


full_periodicity_list <- df_annual$species[which(df_annual$annual_duration==5109)]

save(df_annual,annual_dims,full_periodicity_list,file=)
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
my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(df_annual[order(df_annual$annual_duration,df_annual$func_group),],map,by="func_group")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group

df_annual[order(df_annual$annual_duration,df_annual$func_group),] %>% 
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
  theme(axis.text.y = element_text(colour = color_code))

ggsave(filename=paste0(basepath,"figures/annual_periodicity_bar_all.png"),
       width=1500,height=3000,units="px",dpi=200)


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

ggsave(filename=paste0(basepath,"figures/annual_periodicity_bar.png"),
       width=1600,height=1600,units="px",dpi=200)


df_annual %>% filter(annual_duration <5109)
annual_dims %>% filter(annual_duration <5109)
################################################################################
#ANNUAL TIME RANGE PLOT
################################################################################
annual_dims %>% 
  filter(annual_duration<5109)%>%
  mutate(species=factor(species,levels=unique(species))) %>% 
  ggplot() +
  geom_linerange(aes(x=species,
                     ymin=xmin,ymax=xmax,color=func_group),linewidth=1)+
  coord_flip() + 
  ylab("Year") +
  xlab("Species")+
  scale_color_manual(values=map_dict,name="Functional\nGroup") +
  scale_y_continuous(breaks=seq(0,365*14,365*2),
                     labels=seq(2008,2008+14,2),limits=c(0,365*14))

ggsave(filename=paste0(basepath,"figures/annual_periodicity_line_range.png"),
       width=1000,height=1300,units="px",dpi=100)

#################################################################################
#compute contour dimensions for an INDIVIDUAL WAVELET
#################################################################################
head(annual_dims)


annual_dims = data.frame(species=character(),xmin=numeric(),xmax=numeric())
Colnames <- names(annual_dims)
index = which(protist_tricho_labelC == "Guinardia_delicatula")
plot.biwavelet_adv(super_res[[index]])
wavelet = index
x = super_res[[wavelet]]
species=protist_tricho_labelC[wavelet]
yvals <- log2(super_res[[wavelet]]$period)
tol = 1
contour_pos <- contourLines(x$t, yvals, t(x$signif), level = tol)

annual_contours <- list() #length of wavelets
annual_contours_length <- vector(length=length(contour_pos))

#for each polygon within each wavelet object find which polygons are the annual ones
for(ii in 1:length(contour_pos)){
  #store index in annual contours
  annual_contours[[ii]] <- which((contour_pos[[ii]]$y > 8) & (contour_pos[[ii]]$y < 10))
  #store lengths for each wavelet
  annual_contours_length[ii] <- length(annual_contours[[ii]])
}

annual_index = which(annual_contours_length !=0)
range_i = list()

if(length(annual_index > 0)){
  for(annual_i in 1:length(annual_index)){
    range_i$y = range(contour_pos[[annual_index[annual_i]]]$y)
    range_i$x = range(contour_pos[[annual_index[annual_i]]]$x)
    xmin = range_i$x[1]
    xmax = range_i$x[2]
    if((xmin > (365*14))|xmax<(365*2)){break}
    #removing ranges to account for cone of influence
    xmin = if_else(xmin <= 365*2,0,xmin)
    xmax = pmin(xmax,365*14)
    length_x = xmax-xmin
    wavelet_annual_length = wavelet_annual_length + length_x
    annual_dims <- rbind(annual_dims, c(species,xmin,xmax))
    names(annual_dims) <- Colnames
  }
}

annual_lengths[wavelet] <-wavelet_annual_length


plot.biwavelet_adv(super_res[[index]])
annual_dims[annual_dims$species=="Guinardia_delicatula",]

###############################################
################################################
