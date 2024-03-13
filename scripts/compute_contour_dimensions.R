#PURPOSE: extract contours from wavelet plot and measure x and y dimension
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices","rlist","rapport")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

################################################################################
################################################################################
#COMPUTE CONTOUR LINES FOR ANNUAL PERIOD

#store dimensions of annual contours
annual_dims <- list()
annual_lengths <- vector(length=length(super_res))


annual_dims = data.frame(species=character(),xmin=numeric(),xmax=numeric())
Colnames <- names(annual_dims)

rbind(annual_dims,x)
for(wavelet in 1:length(super_res)){
  print(wavelet)
#extract dimensions of contours
x = super_res[[wavelet]]
species = species_list_i[wavelet]
yvals <- log2(super_res[[wavelet]]$period)
tol = 1
contour_pos <- contourLines(x$t, yvals, t(x$signif), level = tol)

# extract contours at annual band
annual_contours <- list()
annual_contours_length <- vector(length=length(contour_pos))
  for(ii in 1:length(contour_pos)){
    annual_contours[[ii]] <- which((contour_pos[[ii]]$y > 8) & (contour_pos[[ii]]$y < 9))
    annual_contours_length[ii] <- length(annual_contours[[ii]])
  }

#compute length of x and y dimensions for each annual band
ranges = list()
range_i = list()
wavelet_annual_length = 0
annual_index = which(annual_contours_length !=0)

#loop through each polygon with annual if present
if(length(annual_index > 0)){
  for(annual_i in 1:length(annual_index)){
    range_i$y = range(contour_pos[[annual_index[annual_i]]]$y)
    range_i$x = range(contour_pos[[annual_index[annual_i]]]$x)
    length_x = range_i$x[2] - range_i$x[1]
    wavelet_annual_length = wavelet_annual_length + length_x
    annual_dims <- rbind(annual_dims, c(species,range_i$x[1],range_i$x[2]))
    names(annual_dims) <- Colnames
  }
}
annual_lengths[wavelet] <-wavelet_annual_length
}

annual_dims$func_group = NA

#store annual durations in dataframe with species and functional group labels
df_annual = data.frame(annual_duration = annual_lengths,species=species_list_i,func_group = "Unknown")

head(df_annual)

#load ifcb class list file that categories each species in a functional gorup
groups = read.csv(url("https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/IFCB_classlist_type.csv"))
df[sapply(df, is.nan)] <- NA
func_group_list = c("Diatom","Dinoflagellate","Coccolithophore","Ciliate","flagellate")   

#create column with functional group for df annual
for(func_group in func_group_list){
reference=groups$CNN_classlist[(groups[func_group]==1)]
index=intersect(colnames(df),reference)
df_annual[df_annual$species%in%index,"func_group"] = func_group
annual_dims[annual_dims$species%in%index,"func_group"] = func_group

}


#################################################################################
#compute contour dimensiosn for an INDIVIDUAL WAVELET
#################################################################################
which(species_list_i == "Guinardia_delicatula")
plot.biwavelet_adv(super_res[[53]])
wavelet = 53
x = super_res[[wavelet]]
yvals <- log2(super_res[[wavelet]]$period)
tol = 1
contour_pos <- contourLines(x$t, yvals, t(x$signif), level = tol)

annual_contours <- list() #length of wavelets
annual_contours_length <- vector(length=length(contour_pos))

#for each polygon within each wavelet object find which polygons are the annual ones
for(ii in 1:length(contour_pos)){
  #store index in annual contours
  annual_contours[[ii]] <- which((contour_pos[[ii]]$y > 6) & (contour_pos[[ii]]$y < 11))
  #store lengths for each wavelet
  annual_contours_length[ii] <- length(annual_contours[[ii]])
}

annual_index = which(annual_contours_length !=0)
range_i = list()

if(length(annual_index > 0)){
  for(annual_i in 1:length(annual_index)){
    range_i$y = range(contour_pos[[annual_index[annual_i]]]$y)
    range_i$x = range(contour_pos[[annual_index[annual_i]]]$x)
    length_y = range_i$y[2] - range_i$y[1]
    ranges[[annual_i]] <- range_i
    wavelet_annual_length = wavelet_annual_length + length_x
  }
}

annual_lengths[wavelet] <-wavelet_annual_length

df_annual = df_annual[order(df_annual$annual_duration),]

#######################################################
df_annual %>%group_by(func_group) %>%
  summarise(ad_sum = sum(annual_duration),n=n()) %>% 
  ggplot() +
  geom_bar(aes(x=func_group,y=ad_sum/n),stat="identity")+
  xlab("Functional Group") + 
  ylab("Days with Annual Periodicity")


df_annual %>% filter(species!="Chaetoceros_didymus_TAG_external_flagellate",
                     annual_duration > 800) %>%
  mutate(ad = ifelse(annual_duration >=5840,5840,annual_duration))%>%
  arrange(annual_duration) %>% ggplot() +
  geom_bar(aes(x=reorder(species,+ad),y=ad,fill=func_group),
           stat="identity")+
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))+
  ylim(0,5840)+
  scale_fill_discrete(name="Functional\nGroup")+
  ylab("Days with Annual Periodicity")+
  xlab("Species")

ggsave(filename=paste0(basepath,"figures/annual_periodicity_bar.png"),
       width=800,height=800,units="px",dpi=100)

df_annual %>% filter(species!="Chaetoceros_didymus_TAG_external_flagellate") %>% 
  mutate(ad = ifelse(annual_duration >=5840,5840,annual_duration))%>%
  arrange(annual_duration) %>% ggplot() +
  geom_bar(aes(x=reorder(species,+ad),y=ad,fill=func_group),
           stat="identity")+
  coord_flip()+
  ylim(0,5840)+
  scale_fill_discrete(name="Functional\nGroup")+
  ylab("Years with Annual Periodicity")+
  xlab("Species")+
scale_y_continuous(breaks=seq(0,6000,365*2),
                   labels=seq(0,16,2))

ggsave(filename=paste0(basepath,"figures/annual_periodicity_bar_all.png"),
       width=1000,height=1300,units="px",dpi=100)


annual_dims <- annual_dims %>%
  mutate(xmin=as.numeric(xmin),xmax=as.numeric(xmax),range=xmax-xmin)


ggplot(data=annual_dims) +
  geom_linerange(aes(x=reorder(species,+range),ymin=xmin,ymax=xmax,color=func_group),size=1)+
  coord_flip() + 
  ylab("Years with Annual Periodicity") +
  xlab("Species")+
  scale_color_discrete(name="Functional\nGroup")+
  scale_y_continuous(breaks=seq(0,6000,365*2),
                     labels=seq(0,16,2))

ggsave(filename=paste0(basepath,"figures/annual_periodicity_line_range.png"),
       width=1000,height=1300,units="px",dpi=100)
