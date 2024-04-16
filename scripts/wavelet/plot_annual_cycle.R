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

i = 1
week_means  %>% ggplot() + geom_violin(aes_string(x="week",y=full_periodicity_list[i]))


## PLOT TIME SERIES
df_carbonC %>% ggplot() + 
  geom_point(aes_string(x="date",y=protist_tricho_labelC[i]))+
  scale_x_date(date_breaks = "3 years",date_labels="%Y")+
  xlab("Date") + ylab(paste(protist_tricho_labelC[i],"Concentration"))

for (species in full_periodicity_list){
  print(species)
  df_carbonC %>% mutate(week=as.factor(week))%>%
    mutate_at(protist_tricho_labelC,quadroot) %>%
    ggplot() +geom_boxplot(aes_string(x ="week",y = species))+
    xlab("Week") + ylab(expression("4th Root Transformed Carbon Concentration (ug/mL)"^(1/4))) + 
    ggtitle(species) + geom_line(data=week_means,aes_string(x="week",y=species),color="red")
  ggsave(filename=paste0(basepath,"figures/wavelet_transforms/annual_cycle/week_annual_cycle/","week_annual_cycle_",species,".png"),width=800,height=800,units="px",dpi=100)
}

#generate violin plots for week of year
for (species in full_periodicity_list){
  print(species)
  df_carbonC %>% mutate(week=as.factor(week))%>%
    mutate_at(protist_tricho_labelC,quadroot) %>%
   ggplot() +geom_violin(aes_string(x ="week",y = species))+
    xlab("Week") + ylab(expression("4th Root Transformed Carbon Concentration (ug/mL)"^(1/4))) + 
    ggtitle(species) + geom_line(data=week_means,aes_string(x="week",y=species),color="red")
  ggsave(filename=paste0(basepath,"figures/wavelet_transforms/annual_cycle/week_annual_cycle/","week_annual_cycle_",species,".png"),width=800,height=800,units="px",dpi=100)
}

####################
#plot individual
#################
ii = "Acantharia"
df_carbonC  %>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  mutate(week=as.factor(week)) %>%
  ggplot() + 
  geom_boxplot(aes_string(x="week",y=ii))

plot(df_carbonC$date,df_carbonC$detritus^(1/4))


#############################
#############################
fall = c(36:48)
spring = c(10:22)
summer = c(23:35)
winter= c(49:52,1:9)
df_season_sort <- data.frame(species=full_periodicity_list,avg_conc=colMeans(week_means[,full_periodicity_list]),
                             Fall=colMeans(week_means[fall,full_periodicity_list]),
                             Spring=colMeans(week_means[spring,full_periodicity_list]),
                             Summer=colMeans(week_means[summer,full_periodicity_list]),
                             Winter=colMeans(week_means[winter,full_periodicity_list]))

fall_ind = which(df_season_sort$Fall > df_season_sort$avg_conc)
spring_ind = which(df_season_sort$Spring > df_season_sort$avg_conc)
summer_ind = which(df_season_sort$Summer > df_season_sort$avg_conc)
winter_ind = which(df_season_sort$Winter > df_season_sort$avg_conc)

full_periodicity_list[fall_ind]
full_periodicity_list[spring_ind]
full_periodicity_list[summer_ind]
full_periodicity_list[winter_ind]

plot(df_carbonC$date,df_carbonC$Nanoneis,xlab="Date",ylab="Nanoneis Carbon Concentration (ug/ml)")

plot(df_carbonC$date,df_carbonC$Leptocylindrus,xlab="Date",ylab="Leptocylinderus Carbon Concentration (ug/ml)")+
  scale_x_discrete(date_breaks="2 year",label)

###############################################
df_carbonC$year <- year(df_carbonC$date)
df_carbonC$week <- week(df_carbonC$date)
df_carbonC$wyear <- paste0(df_carbonC$week,"-",df_carbonC$year)

df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

df_carbonC_wyear_mean$year <- factor(df_carbonC_wyear_mean$year)

ggplot(data=df_carbonC_wyear_mean) + geom_line(aes(x=week,y=Leptocylindrus,
                                         color = year)) +
  xlab("Week of Year") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"^(1/4)))+
  ggtitle("Leptocylindrus")

ggplot(data=df_carbonC_wyear_mean) + geom_line(aes(x=week,y=Gonyaulax,
                                                   color = year)) +
  xlab("Week of Year") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"^(1/4)))+
  ggtitle("Gonyaulax")

for(i in protist_tricho_labelC){
  print(i)
df_carbonC_wyear_mean %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  ggplot() + geom_line(aes_string(x="week",y=i,color="year"))+
    xlab("Week") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  ggtitle(i)+scale_x_continuous(breaks=seq(1,53,4))+
  ylim(0,20)
  ggsave(filename = paste0(basepath,"/figures/wavelet_transforms/annual_cycle/week_year_quadroot/week_year_annual_cycle_",i,".png"),
         width=600,height=500,units="px",dpi=100)
}
i = "Tripos_furca"
df_carbonC_wyear_mean %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  ggplot() + geom_line(aes_string(x="week",y=i,color="year"))+
  xlab("Week") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  ggtitle(i)+scale_x_continuous(breaks=seq(1,53,4))+
  ylim(0,20)
ggsave(filename = paste0(basepath,"/figures/wavelet_transforms/annual_cycle/week_year_quadroot/week_year_annual_cycle_",i,".png"),
       width=600,height=500,units="px",dpi=100)

for(i in full_periodicity_list){
  print(i)
  df_carbonC_wyear_mean %>% 
    ggplot() + geom_line(aes_string(x="week",y=i,color="year"))+
    xlab("Week") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
    ggtitle(i)+scale_x_continuous(breaks=seq(1,53,4))+
    ylim(0,20)
  ggsave(filename = paste0(basepath,"/figures/wavelet_transforms/annual_cycle/week_year/week_year_annual_cycle_",i,".png"),
         width=600,height=500,units="px",dpi=100)
}
i = 1

###########################################################################
#
###################################################################################
week_means %>% select(contains("Chaetoceros"),week,-Chaetoceros) %>%
  gather(key=species,value=carbon_conc,-c(week)) %>%
  ggplot() + geom_line(aes(x=week,y=carbon_conc^(1/4),color = species)) +
  xlab("Week of Year") +
  ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"^(1/4)))+
  scale_x_continuous(breaks=seq(0,53,4))+
  ylim(0,1.5)

ggsave(filename = paste0(basepath,"/figures/cyclic_index/chaetoceros_week_mean_concentration.png"),
       width=800,height=500,units="px",dpi=100)


df_cor %>%  select(contains("Chaetoceros"),year,-Chaetoceros) %>%
  gather(key=species,value=corr,-c(year)) %>%
  ggplot() + geom_line(aes(x=year,y=corr,color=species)) +
  geom_point(aes(x=year,y=corr,color=species,shape=species)) +
  geom_hline(aes(yintercept=0),color="black",linetype="dashed")+
  ylim(-1,1) +
  scale_x_continuous(breaks=seq(2006,2023,2))+
  xlab("Year") + ylab("Correlation")

ggsave(filename = paste0(basepath,"/figures/cyclic_index/chaetoceros_correlation_over_time.png"),
       width=800,height=500,units="px",dpi=100)

#############################################################
#Tripos Bloom
##############################################################
df_carbonC %>% select(contains("Tripos"),-contains("Dino"),date,year) %>%
  filter(year==2023) %>%
  gather(key=species,value=conc,-c(date,year))%>%
  ggplot() + geom_line(aes(x=date,y=conc,color=species),alpha=0.7)+
  ggtitle("2023 Tripos Bloom")

