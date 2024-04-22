basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
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
df_dtw <- as.data.frame(matrix(nrow=0,ncol=length(protist_tricho_labelC)+1))
names(df_cor) <- c("year",protist_tricho_labelC)
names(df_dtw) <- c("year",protist_tricho_labelC)
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length(protist_tricho_labelC)+1))
names(annual_peak) <- c("year",protist_tricho_labelC)

#loop through the years and store colreation
for(y in 1:length(years)){
  print(years[y])
  dtw_distance = rep(NA,times=length(protist_tricho_labelC))
  #extract week year means of a specific year
  individual_year <- ref_year_interp %>% ungroup() %>% filter(year == years[y]) %>% select(protist_tricho_labelC)
  #extract diagonal of the correlation matrix
  correlation= diag(cor(week_climatology,individual_year))
  for(i in 1:length(protist_tricho_labelC)){
    dtw_distance[i] <- dtw(individual_year[,i],week_climatology[,i])$normalizedDistance
  } 
  append_this_cor <- as.data.frame(t(c(year=years[y],correlation)))
  append_this_dtw <- as.data.frame(t(c(year=years[y],dtw_distance)))
  #append to dataframe
  df_cor <- rbind(df_cor,append_this_cor)
  df_dtw <- rbind(df_dtw,append_this_dtw)
  annual_peak <- rbind(annual_peak,c(year=years[y],sapply(individual_year, max, na.rm = TRUE)))
}

names(annual_peak) <- c("year",protist_tricho_labelC)
names(df_dtw) <- c("year",protist_tricho_labelC)

x = seq(1,40,1)
noise = rnorm(length(x),0,sd=0.1)
y=sin(x)
z= sin(x) + 1
dtw(y,z)$normalizedDistance

#take mean of cyclic index
c_index_median = apply(df_cor[,protist_tricho_labelC],2,median,na.rm=T)
c_index_sd <- apply(df_cor[,protist_tricho_labelC], 2, sd,na.rm=T)
c_index_median_dtw = apply(df_dtw[,protist_tricho_labelC],2,median,na.rm=T)
consistency.fun <- function(annual_peak){1 - sd(annual_peak - mean(annual_peak))/mean(annual_peak)}
consistency_index <- apply(annual_peak[,protist_tricho_labelC],2,consistency.fun)

c_index = data.frame(cyclicity_index=c_index_median,sd=c_index_sd,
                     consistency = consistency_index,
                     dtw = c_index_median_dtw)
c_index$species <- rownames(c_index)

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


#PLOT individual correlation over time
ii = which(protist_tricho_labelC=="Tripos")
ggplot(data=df_cor) + geom_point(aes_string(x="year",y=protist_tricho_labelC[ii]))+
  geom_segment(aes_string(x="year",y=0,xend="year",yend=protist_tricho_labelC[ii]))+
  geom_hline(aes(yintercept=0),color="black")+
scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
  ylab("Correlation Coefficient") +xlab("Year")+
  ggtitle(protist_tricho_labelC[ii])



###################################################################################
#plot c_index
##################################################################################

#for colorcoding text by functional group
my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(c_index[order(c_index$cyclicity_index,c_index$func_group),],map,by="func_group")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group


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
#DTW
#############################

my_colors <- RColorBrewer::brewer.pal(6, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(c_index[order(c_index$dtw,c_index$func_group),],map,by="func_group")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group

c_index %>% ggplot() +
  geom_bar(aes(x=reorder(species,+dtw),y=dtw,fill=as.factor(func_group)),
           stat="identity")+
  coord_flip()+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  xlab("dtw Distance") + ylab("Species")+
  theme(axis.text.y = element_text(colour = color_code))


ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_quadroot_median_dtw.png"),
       width=1500,height=3000,units="px",dpi=200)

