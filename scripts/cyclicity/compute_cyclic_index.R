basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC_filled.RData"))

#add date time objects
df_carbonC$doy_numeric <- yday(df_carbonC$date)
df_carbonC$week <- week(df_carbonC$date)
quadroot <- function(x){x^(1/4)}

head(df_carbonC)

#compute day of year and weekly means 
doy_means <- df_carbonC %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  group_by(doy_numeric) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

#compute weekly mean
week_means <- df_carbonC %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  group_by(week) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

df_carbonC$year <- year(df_carbonC$date)
df_carbonC$wyear <- paste0(df_carbonC$week,"-",df_carbonC$year)
#compute mean at every week year

df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()
df_carbonC_wyear_mean$year <- factor(df_carbonC_wyear_mean$year)

###################################################################
#Compute cyclic index
#############################################################
#create complete week year index (will left join with dataframe later)
week <- seq(1,53,1)
years <- seq(min(df_carbonC$year),max(df_carbonC$year),1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=as.factor(year_list))

#approximate annual cycle with interpolation 
ref_year_interp <- df_carbonC_wyear_mean%>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate_at(protist_tricho_labelC,
            list(~na.approx(object=.,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear"))) 


#average weekly annual cycle across entire time series
week_climatology = week_means %>% select(protist_tricho_labelC)

#create dataframe to store correlations
df_cor <- as.data.frame(matrix(nrow=0,ncol=length(protist_tricho_labelC)+1))
names(df_cor) <- c("year",protist_tricho_labelC)
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length(protist_tricho_labelC)+1))
names(annual_peak) <- c("year",protist_tricho_labelC)

#loop through the years and store colreation
for(y in 1:length(years)){
  print(years[y])
  #extract week year means of a specific year
  individual_year <- ref_year_interp %>% ungroup() %>% filter(year == years[y]) %>% select(protist_tricho_labelC)
  #extract diagonal of the correlation matrix
  correlation= diag(cor(week_climatology,individual_year))
  append_this <- as.data.frame(t(c(year=years[y],correlation)))
  #append to dataframe
  df_cor <- rbind(df_cor,append_this)
  annual_peak <- rbind(annual_peak,c(year=years[y],sapply(individual_year, max, na.rm = TRUE)))
}

names(annual_peak) <- c("year",protist_tricho_labelC)

#take mean of cyclic index
c_index_median = apply(df_cor[,protist_tricho_labelC],2,median,na.rm=T)
c_index_sd <- apply(df_cor[,protist_tricho_labelC], 2, sd,na.rm=T)

consistency.fun <- function(annual_peak){1 - sd(annual_peak - mean(annual_peak))/mean(annual_peak)}
consistency_index <- apply(annual_peak[,protist_tricho_labelC],2,consistency.fun)

c_index = data.frame(cyclicity_index=c_index_median,sd=c_index_sd,consistency = consistency_index)
c_index$species <- rownames(c_index)

################################################
#plot correlation over time for each species
###############################################
for(ii in 1:length(protist_tricho_labelC)){
  print(paste0(ii," of ",length(protist_tricho_labelC)))
  ggplot(data=df_cor) + geom_point(aes_string(x="year",y=protist_tricho_labelC[ii]))+
    geom_segment(aes_string(x="year",y=0,xend="year",yend=protist_tricho_labelC[ii]))+
    geom_hline(aes(yintercept=0),color="black")+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
  ylab("Correlation Coefficient") +xlab("Year")+
  ggtitle(protist_tricho_labelC[ii])
ggsave(filename=paste0(basepath,"/figures/cyclic_index/correlation_over_time_quadroot/cyclic_correlation_over_time_",protist_tricho_labelC[ii],".png"),
       width=600,height=500,units="px",dpi=120)
}


#PLOT individual correlation over time
ii = which(protist_tricho_labelC=="Tripos")
ggplot(data=df_cor) + geom_point(aes_string(x="year",y=protist_tricho_labelC[ii]))+
  geom_segment(aes_string(x="year",y=0,xend="year",yend=protist_tricho_labelC[ii]))+
  geom_hline(aes(yintercept=0),color="black")+
scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
  ylab("Correlation Coefficient") +xlab("Year")+
  ggtitle(protist_tricho_labelC[ii])

################
#GUINARDIA VIGNETTE = STANDARD DEVIATON
#############

df_cor_long <-df_cor %>% gather(species,cor,-year) %>% drop_na() %>% 
  filter(str_detect(species, "Guinardia"))


aov_res <- aov(cor ~ species, data=df_cor_long) 

aov_res%>% ggplot(aes(x = species, y = cor)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Group") + ylab("Scores")+
  geom_signif(comparisons = list(c("group_1", "group_2","group_3")), 
              map_signif_level=TRUE)

#add functional group column to cyclic index object
c_index$func_group = "Unknown"
#load ifcb class list file that categories each species in a functional group
func_group_list = c("Diatom","Dinoflagellate","Ciliate","Nano-Flag-Cocco","Metazoan","Unknown")
func_group_labels <- list(diatom_labelC,dino_label,ciliate_label,nanoflagcocco_label,metazoan_label,c("Unknown"))
#create column with functional group 
for(func_group in 1:length(func_group_list)){
  reference=func_group_labels[[func_group]]
  c_index[c_index$species%in%reference,"func_group"] = func_group_list[func_group]
}

#for colorcoding text by functional group
my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(c_index[order(c_index$cyclicity_index,c_index$func_group),],map,by="func_group")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group

###################################################################################
#plot c_index
##################################################################################
c_index %>%
ggplot() + geom_bar(aes(x=reorder(species,+cyclicity_index),
                        y=cyclicity_index,
                        fill=func_group),
                                stat="identity")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  coord_flip()+ylim(-0.05,1)+
  ylab("Cyclicity Index")+xlab("Species")+
  theme(axis.text.y = element_text(colour = color_code))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_quadroot_median.png"),
       width=1500,height=3000,units="px",dpi=200)

#################################################################################
# Standard Deviation Bar Plot
#################################################################################
ggplot(data=c_index) + geom_bar(aes(x=reorder(species,+sd),
                                    y=sd,
                                    fill=func_group),
                                stat="identity")+
  coord_flip()+
  ylab("Standard Deviation")+xlab("Species")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")

ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_sd.png"),
       width=1500,height=3000,units="px",dpi=200)

############################
#consistency
#############################
c_index %>% ggplot() +
  geom_bar(aes(x=reorder(species,+consistency),y=consistency,fill=func_group),
           stat="identity")+
  coord_flip()+ ylim(0,1)+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  xlab("Consistency Index") + ylab("Species")


################################################################################
#### HISTOGRAM of cyclity
###############################################################################
bin_count = 12
set.seed(7)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colors = sample(cbbPalette,4)

ggplot() +
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
ggplot() +
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


#########################################################################
#Histogram of Consistency in Annual Peak
########################################################################

ggplot() +
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


#plot Annual Peak over Time

ggplot(data = annual_peak) + geom_point(aes(x = year,y= Ephemera))

##########################################
# Trip0s vignette
#############################################

