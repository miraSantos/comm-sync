basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "astsa","dtw","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/df_carbon_2024_Mar_24.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2023_Mar_26_df_carbon_labels.RData"))

#add date time objects
df_carbonC$doy_numeric <- yday(df_carbonC$date)
df_carbonC$week <- week(df_carbonC$date)
quadroot <- function(x){x^(1/4)}

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
year <- seq(2006,2023,1)
week_list <- rep(week,length(year))
year_list <- rep(year,53)[order(rep(year,53))]
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=as.factor(year_list))

#approximate annual cycle with interpolation 
ref_year_interp <- df_carbonC_wyear_mean%>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate_at(full_periodicity_list,
            list(~na.approx(object=.,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear"))) 


#average weekly annual cycle across entire time series
week_climatology = week_means %>% select(full_periodicity_list)

#create dataframe to store correlations
df_cor <- as.data.frame(matrix(nrow=0,ncol=length(full_periodicity_list)+1))
names(df_cor) <- c("year",full_periodicity_list)
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length(full_periodicity_list)+1))
names(annual_peak) <- c("year",full_periodicity_list)
#loop through the years and store colreation
for(y in year){
  print(y)
  #extract week year means of a specific year
  individual_year <- ref_year_interp %>% ungroup() %>% filter(year == y) %>% select(full_periodicity_list)
  #extract diagonal of the correlation matrix
  t= diag(cor(week_climatology,individual_year))
  #append to dataframe
  df_cor <- rbind(df_cor,as.data.frame(t(c(year=y,t))))
  annual_peak <- rbind(annual_peak,c(year=y,sapply(individual_year, max, na.rm = TRUE)))
  t=NA
}

#take mean of cyclic index
c_index = colMeans(abs((df_cor[full_periodicity_list])))
c_index = data.frame(c_index)
c_index$species <- rownames(c_index)

#plot correlation over time for each species
for(ii in 1:length(full_periodicity_list)){
  print(paste0(ii," of ",length(full_periodicity_list)))
ggplot(data=df_cor) + geom_line(aes_string(x="year",y=full_periodicity_list[ii]))+
  geom_point(aes_string(x="year",y=full_periodicity_list[ii]))+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
  ylab("Correlation Coefficient") +xlab("Year")+
  ggtitle(full_periodicity_list[ii])
ggsave(filename=paste0(basepath,"/figures/cyclic_index/correlation_over_time_quadroot/cyclic_correlation_over_time_",full_periodicity_list[ii],".png"),
       width=600,height=500,units="px",dpi=120)
}

#PLOT individual correlation over time
ii = which(full_periodicity_list=="Tripos")
ggplot(data=df_cor) + geom_line(aes_string(x="year",y=full_periodicity_list[ii]))+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
  ylab("Correlation Coefficient") +xlab("Year")+
  ggtitle(full_periodicity_list[ii])

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
color_code = left_join(c_index[order(c_index$c_index,c_index$func_group),],map,by="func_group")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group

#plot c_index
c_index %>% drop_na() %>%
ggplot() + geom_bar(aes(x=reorder(species,+c_index),y=c_index,fill=func_group),
                                stat="identity")+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  coord_flip()+ylim(0,1)+
  ylab("Cyclicity Index")+xlab("Species")+
  theme(axis.text.y = element_text(colour = color_code))


ggsave(filename=paste0(basepath,"/figures/cyclic_index/cyclic_index_quadroot.png"),
       width=1500,height=3000,units="px",dpi=200)

#plot individual interpolation
y = 2023
test <- ref_year_interp %>% ungroup() %>% filter(year == y) %>% select(full_periodicity_list)
df_carbonC_wyear_mean %>% filter(year==y) %>% ggplot() +
  geom_point(aes_string(x=week,y=))+
  geom_point(data=ref_year_interp[ref_year_interp$year==y,],aes(x=week,y=Acantharia,color="red"),shape=3)+
  geom_point(data=test,aes(x=week,Acantharia),shape=3,color="blue")



#plot Annual Peak over Time

ggplot(data = annual_peak) + geom_point(aes(x = year,y= Ephemera))