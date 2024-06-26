basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "astsa","dtw","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"/data/r_objects/filled/2024-06-13_df_carbonC_filled_merged.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))

df_carbonC$doy_numeric <- yday(df_carbonC$date)
df_carbonC$week <- as.factor(week(df_carbonC$date))
quadroot <- function(x){x^(1/4)}

#compute day of year and weekly means 
doy_means <- df_carbonC %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
 group_by(doy_numeric) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

week_means <- df_carbonC %>% 
  group_by(week) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

## PLOT TIME SERIES
df_carbonC %>% ggplot() + 
  geom_point(aes_string(x="date",y=protist_tricho_labelC[i]))+
  scale_x_date(date_breaks = "3 years",date_labels="%Y")+
  xlab("Date") + ylab(paste(protist_tricho_labelC[i],"Concentration"))


taxa_i = label_maybe_include[1]
for (taxa_i in label_maybe_include){
  print(taxa_i)
  df_carbonC %>% mutate(week=as.factor(week))%>%
    mutate_at(protist_tricho_labelC,quadroot) %>%
    ggplot() +geom_boxplot(aes_string(x ="week",y = taxa_i))+
    xlab("Week") +
    ylab(expression("4th Root Transformed Carbon Concentration (ug/mL)"^(1/4))) + 
    ggtitle(taxa_i) + geom_line(data=week_means,
                                aes_string(x="week",y=taxa_i),
                                color="red")
  ggsave(filename=paste0(basepath,"figures/annual_cycle/week_annual_cycle/","week_annual_cycle_",species,".png"),width=800,height=800,units="px",dpi=100)
}

#generate violin plots for week of year
for (species in label_maybe_include){
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
ii = "Emiliania_huxleyi"
ii = "Leptocylindrus"
ii = "Tripos_fusus"

df_carbonC_wyear_mean  %>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  ggplot() + 
  geom_line(aes_string(x="week",y=ii,color="year"))


df_carbonC_wyear_mean %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  ggplot() + geom_point(aes_string(x="date",y=ii))

plot(df_carbonC$date,df_carbonC$detritus^(1/4))


#############################
#############################
fall = c(36:48)
spring = c(10:22)
summer = c(23:35)
winter= c(49:52,1:9)
df_season_sort <- data.frame(species=label_maybe_include,avg_conc=colMeans(week_means[,label_maybe_include]),
                             Fall=colMeans(week_means[fall,label_maybe_include]),
                             Spring=colMeans(week_means[spring,label_maybe_include]),
                             Summer=colMeans(week_means[summer,label_maybe_include]),
                             Winter=colMeans(week_means[winter,label_maybe_include]))

fall_ind = which(df_season_sort$Fall > df_season_sort$avg_conc)
spring_ind = which(df_season_sort$Spring > df_season_sort$avg_conc)
summer_ind = which(df_season_sort$Summer > df_season_sort$avg_conc)
winter_ind = which(df_season_sort$Winter > df_season_sort$avg_conc)

label_maybe_include[fall_ind]
label_maybe_include[spring_ind]
label_maybe_include[summer_ind]
label_maybe_include[winter_ind]

plot(df_carbonC$date,df_carbonC$Nanoneis,xlab="Date",ylab="Nanoneis Carbon Concentration (ug/ml)")

plot(df_carbonC$date,df_carbonC$Leptocylindrus,xlab="Date",ylab="Leptocylinderus Carbon Concentration (ug/ml)")+
  scale_x_discrete(date_breaks="2 year",label)

###############################################
df_carbonC$year <- year(df_carbonC$date)
df_carbonC$week <- week(df_carbonC$date)
df_carbonC$wyear <- paste0(df_carbonC$week,"-",df_carbonC$year)

df_carbonC_wyear_mean <-df_carbonC %>% mutate(year=year(date),
                                              week=week(date),
                                              wyear=paste0(year,"-",week)) %>%
  group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

df_carbonC_wyear_mean$year <- factor(df_carbonC_wyear_mean$year)


#Generate plots 
i = protist_tricho_labelC[1]
for(i in protist_tricho_labelC){
  print(i)
df_carbonC_wyear_mean %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  ggplot() + geom_line(aes_string(x="week",y=i,color="year"))+
    xlab("Week") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  ggtitle(i)+scale_x_continuous(breaks=seq(1,53,4))+
  ylim(0,20)
  ggsave(filename = paste0(basepath,"/figures/annual_cycle/week_year_quadroot/week_year_annual_cycle_",i,"_",Sys.Date(),".png"),
         width=600,height=500,units="px",dpi=100)
}

for(i in label_maybe_include){
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

##################################################################
#plot as polar plot
###############################################################
df_carbonC_wyear_mean <-df_carbonC %>% mutate(year=year(date),
                                              week=week(date),
                                              wyear=paste0(year,"-",week)) %>%
  group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

df_carbonC_filled_trimmed$week <- week(df_carbonC_filled_trimmed$date)
df_carbonC_filled_trimmed %>%
  mutate_at(label_maybe_include,log10)%>%
  ggplot(aes_string(x="doy_numeric", y=taxa_i))+
  coord_polar(theta="x", start=0,direction=-1)+
  geom_line(aes(color=as.factor(year)))

for(taxa_i in label_maybe_include){
  print(taxa_i)
  df_carbonC_filled_trimmed %>%
  mutate_at(label_maybe_include,log10)%>%
  ggplot(aes_string(x="doy_numeric", y=taxa_i))+
  coord_polar(theta="x", start=0,direction=1)+
  geom_line(aes(color=as.factor(year)),alpha=0.5)+
    guides(color=guide_legend(ncol=2))+
    labs(x="Day of Year",
         y=expression("log-transformed Carbon Concentration ("*mu*"g ml"^-1*")"),
         color="Year")+
    scale_x_continuous(breaks=seq(0,365,30))
  
ggsave(filename=paste0(basepath,"/figures/annual_cycle/polar_plot/polar_plot_",Sys.Date(),"_",taxa_i,".png"),
       width=1000,height=800,units="px",dpi=150)
}