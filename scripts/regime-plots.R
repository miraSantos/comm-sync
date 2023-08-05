save_r_path = "C:\\Users\\Miraflor P Santos\\comm-sync\\data\\ifcb\\r_objects\\unfilled\\"
load(paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
load(paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

################################################################################
# COMPUTE SUMMER ANOMALY?
dfcarbon_means = dfcarbon_group %>% group_by(doy_numeric)%>%
  summarize(mean_diatom=mean(Diatom_noDetritus,na.rm=T))

#compute anomaly
dfcarbon_weekly <- dfcarbon_group %>%
  left_join(dfcarbon_means, by = "doy_numeric") %>%
  mutate(diatom_minus_mean = Diatom_noDetritus-mean_diatom)


################################################################################
#Plotting time series 
ggplot(data=dfcarbon_group, aes(x=date))+
  geom_point(aes(y=AvgWindSpeed))+
  scale_x_date(date_breaks="1 year", date_labels="%Y")+
  ylab("Wind Speed (knots")


################################################################################
# DIVIDE DATA INTO REGIMES
#creating indices to indicate regimes
dfcarbon_group$regime = NaN

regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfcarbon_group$year < regime_1_end))
regime_2_index = (which((dfcarbon_group$year >= regime_1_end)&(dfcarbon_group$year < regime_2_end)))
regime_3_index = (which(dfcarbon_group$year >= regime_2_end))

dfcarbon_group$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfcarbon_group$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfcarbon_group$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")





#################### PLOTTING REGIMES, FUNCTIONAL GROUPS at WEEKLY LEVEL

ggplot(data=dfcarbon_group,aes(x = month,y = protist_tricho))+
  geom_boxplot(color="darkgreen",outlier.color="black")+ 
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Protist Carbon Concentration")+ylim(0,2.5e5)+
  theme(text = element_text(size = 15))

ggplot(data=dfcarbon_group,aes(x = week,y = Diatom_noDetritus)) + 
  geom_boxplot(color="darkgreen",outlier.color="black")+ 
  facet_grid(cols=vars(regime))+
  xlab("Week of Year")+
  scale_x_discrete(breaks=seq(1,52,5))+
  ylab(paste0(group_list[1]," Carbon Concentration (ug/mL)"))+ylim(0,1.25e5)+
  theme(text = element_text(size = 15))


ggplot(data=dfcarbon_group,aes(x = week,y = Ciliate))+
  geom_boxplot(color="darkgreen",outlier.color="black")+ 
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Ciliate Carbon Concentration")+
  theme(text = element_text(size = 15))

ggplot(data=dfcarbon_group,aes(x = week,y = (Dinoflagellate))) +
  geom_boxplot(color="darkgreen",outlier.color="black")+ 
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Dinoflagellate Carbon Concentration")+ylim(0,5e4)+
  theme(text = element_text(size = 15))

ggplot(data=dfcarbon_group,aes(x = week,y = NanoFlagCocco))+
  geom_boxplot(color="darkgreen",outlier.color="black")+ 
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("NanoFlagCocco Carbon Concentration")+
  theme(text = element_text(size = 15))+
  ylim(0,2e5)


################################ PLOTTING ENVIRONMENTAL VARIABLES
#computing mean doy temperature
dfcarbon_group_doy =dfcarbon_group %>% group_by(doy_numeric) %>% summarize(across(all_of(c("Dinoflagellate","Diatom_noDetritus","Ciliate"))))

ggplot(data=dfcarbon_group,aes(x = doy_numeric,y = (Beam_temperature_corrected))) +
  geom_point(alpha=0.2,color="red")+
  geom_line(data=dfcarbon_group_doy,aes(x=doy_numeric,y=mean_temp),color="black")+
  facet_grid(cols=vars(regime))+
  xlab("Day of Year")+
  ylab("Temperature (Deg C)")+
  theme(text = element_text(size = 15))


ggplot(data=dfcarbon_group,aes(x = month,y = Beam_temperature_corrected)) +
  geom_boxplot(color="darkred")+
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,12,1))+
  xlab("Month of Year")+
  ylab("Temperature")

ggplot(data=dfcarbon_group,aes(x = doy_numeric,y = (solar))) +
  geom_point(alpha=0.2,color="orange")+
  facet_grid(cols=vars(regime))+
  xlab("Day of Year")+
  ylab("Solar Irradiance")+
  theme(text = element_text(size = 15))


ggplot(data=dfcarbon_group,aes(x = week,y = (AvgWindSpeed))) +
  geom_boxplot(alpha=0.2,color="blue")+
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Wind Speed")+
  theme(text = element_text(size = 15))

ggplot(data=dfcarbon_group,aes(x = week,y = (AvgWindDir))) +
  geom_boxplot(alpha=0.2,color="blue")+
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Wind Direction")+
  theme(text = element_text(size = 15))



############################## PLOTTING SPECIFIC YEARS
year = 2018
ggplot(data=dfcarbon_group[dfcarbon_group$year == year,],aes(x = doy_numeric))+
  geom_point(aes(y=scale(Diatom_noDetritus),color="Diatoms"))+
  geom_point(aes(y = (scale(Dinoflagellate)),color="Dinos"))+
  ylim(-3,3)



############# MEDIAN SALINITY

df_sal$doy_numeric = yday(df_sal$date)
dfsal_ms = df_sal %>% group_by(regime,doy_numeric) %>% 
  mutate(median_salinity = median(salt)) %>%
  ungroup()

# Median salinity by day of year grouped by each regime (color) 
ggplot(data = df_sal) +
  geom_line(aes(x = doy_numeric,y = median_salinity,color=regime),size = 1)+
  scale_x_continuous(breaks=seq(0,366,30))+
  xlab("Day of Year")+
  ylab("Median Salinity (psu)")

ggplot(data = dfcarbon_group_ms) +
  geom_point(aes(x = doy_numeric,y = median_AvgWindSpeed,color=regime),size = 1)+
  scale_x_continuous(breaks=seq(0,366,30))+
  xlab("Day of Year")+
  ylab("Median Wind Speed (knots)")

figure_save_path = "C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\salinity\\"

ggsave(filename=paste0(figure_save_path,
                       "median-salinity-regime-MVCO.png"),
       width=800,height=400,units="px",dpi=150)


