basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "astsa","dtw","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/df_carbon_2024_Mar_24.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2023_Mar_26_df_carbon_labels.RData"))

df_carbonC$doy_numeric <- yday(df_carbonC$date)
df_carbonC$week <- week(df_carbonC$date)
quadroot <- function(x){x^(1/4)}

#compute day of year and weekly means 
doy_means <- df_carbonC %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  group_by(doy_numeric) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

week_means <- df_carbonC %>% 
  group_by(week) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

df_carbonC$year <- year(df_carbonC$date)
df_carbonC$week <- week(df_carbonC$date)
df_carbonC$wyear <- paste0(df_carbonC$week,"-",df_carbonC$year)

df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

df_carbonC_wyear_mean$year <- factor(df_carbonC_wyear_mean$year)

###################################################################
#Compute cyclic index
#############################################################
week <- seq(1,53,1)
year <- seq(2006,2023,1)
week_list <- rep(week,length(year))
year_list <- rep(year,53)[order(rep(year,53))]

dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=as.factor(year_list))

ref_year_interp <- df_carbonC_wyear_mean%>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate_at(full_periodicity_list,
            list(~na.approx(object=.,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear"))) 


week_climatology = week_means %>% select(full_periodicity_list)
df_cor <- as.data.frame(matrix(nrow=0,ncol=length(full_periodicity_list)+1))
names(df_cor) <- c("year",full_periodicity_list)

for(y in year){
  print(y)
  test <- ref_year_interp %>% ungroup() %>% filter(year == y) %>% select(full_periodicity_list)
  t= diag(cor(week_climatology,test))
  df_cor <- rbind(df_cor,as.data.frame(t(c(year=y,t))))
  t=NA
}

c_index = colMeans(df_cor[full_periodicity_list])
c_index = data.frame(c_index)
c_index$species <- rownames(c_index)
ggplot(data=c_index) + geom_bar(aes(x=reorder(species,+c_index),y=c_index),stat="identity")+
  coord_flip()+ylim(0,1)

#plot individual interpolation
y = 2023
test <- ref_year_interp %>% ungroup() %>% filter(year == y) %>% select(full_periodicity_list)
df_carbonC_wyear_mean %>% filter(year==y) %>% ggplot() +
  geom_point(aes(x=week,y=Acantharia))+
  geom_point(data=ref_year_interp[ref_year_interp$year==y,],aes(x=week,y=Acantharia,color="red"),shape=3)+
  geom_point(data=test,aes(x=week,Acantharia),shape=3,color="blue")

