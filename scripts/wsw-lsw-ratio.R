list.of.packages <- c("ggplot2","remotes","knitr","rmarkdown",
                      "readxl","lubridate","dplyr","sf")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)
#see technical documentation here: https://noaa-edab.github.io/tech-doc/

######################
#Ratio of WSW to LSW
################### 
WSW = ecodata::slopewater[ecodata::slopewater$Var=="WSW proportion ne channel",]
LSW = ecodata::slopewater[ecodata::slopewater$Var=="LSLW proportion ne channel",]

df_sw = data.frame(time=WSW$Time,WSW=WSW$Value,LSW = LSW$Value)

dfsw_long <- gather(df_sw,slopewater,value,WSW:LSW)

time_range = which(dfsw_long$time >= 2006)
ggplot(data = dfsw_long[time_range,])+
  geom_col(aes(x = time, y = value,fill = slopewater),size = 2)+
  scale_x_continuous(breaks = seq(2006,2021,1))+
  ggtitle("LSW and WSW Proportion")+
  ylab("Percent of Total Slopewater (%)")+
  xlab("Year")

ggsave("/home/mira/MIT-WHOI/github_repos/comm-sync/figures/environmental/wsw-lsw-ratio.png")


######################
#AGGREGATE BIOMASS
##################

head(ecodata::aggregate_biomass)
ab_index = which(ecodata::aggregate_biomass$Var == "NA Spring Biomass Index")

ggplot(data = ecodata::aggregate_biomass[ab_index,]) + geom_point(aes(x=Time,y=Value))



######################
#GULFSTREAM INDEX
##################### 
#GULF STREAM INDEX

ecodata::ESP_gsi



