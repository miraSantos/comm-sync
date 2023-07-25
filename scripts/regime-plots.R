load("/dos/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_19_dfcarbon_group.RData")
load("/dos/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_19_dfcount_index.RData")
load("/dos/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_19_dfnut.RData")
list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","ggbump")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/dos/MIT-WHOI/community_sychrony/scripts//adv_biwavelet_packages.R")
rm(wt)

#compute date values 
dfcarbon_group$year = year(dfcarbon_group$date)
dfcarbon_group$week =factor(week(dfcarbon_group$date))
dfcarbon_group$month = factor(month(dfcarbon_group$date))
dfcarbon_group$regime = NaN


######################################### COMPUTE SUMMER ANOMALY?
dfcarbon_means = dfcarbon_group %>% group_by(doy_numeric)%>%
  summarize(mean_diatom=mean(Diatom_noDetritus,na.rm=T))

dfcarbon_weekly <- dfcarbon_group %>%
  left_join(dfcarbon_means, by = "doy_numeric") %>%
  mutate(diatom_minus_mean = Diatom_noDetritus-mean_diatom)


#creating indices to indicate regimes

regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfcarbon_group$year < regime_1_end))
regime_2_index = (which((dfcarbon_group$year >= regime_1_end)&(dfcarbon_group$year < regime_2_end)))
regime_3_index = (which(dfcarbon_group$year >= regime_2_end))

dfcarbon_group$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfcarbon_group$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfcarbon_group$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")


ggplot(data=dfcarbon_group, aes(x=date))+
  geom_point(aes(y=Diatom_noDetritus))+
  scale_x_date(date_breaks="1 year", date_labels="%Y")



################################################################################

#compute regime by year

ggplot(data=dfcarbon_group,aes(x=doy_numeric))+
  geom_line(aes(y=Diatom_noDetritus,color=factor(year)))+
  facet_grid(cols=vars(regime))+
  ylim(0,1.5e5)+
  scale_colour_brewer(palette = "Paired")+
  xlab("Day of Year")+
  ylab("Diatom Carbon Concentration ug/mL")


#################### PLOTTING REGIMES, FUNCTIONAL GROUPS at WEEKLY LEVEL
ggplot(data=dfcarbon_group,aes(x = week,y = Diatom_noDetritus)) + 
  geom_boxplot(color="darkgreen",outlier.color="black")+ 
  facet_grid(cols=vars(regime))+
  xlab("Week of Year")+
  scale_x_discrete(breaks=seq(1,52,5))+
  ylab("Diatom Carbon Concentration (ug/mL)")+ylim(0,1.25e5)+
  theme(text = element_text(size = 15))

ggplot(data=dfcarbon_group,aes(x = week,y = protist_tricho))+
  geom_boxplot(color="darkgreen",outlier.color="black")+ 
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Protist Carbon Concentration")+ylim(0,2.5e5)+
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

############################ PLOT YEARS IN DIFFERENT COLORS

################################ PLOTTING ENVIRONMENTAL VARIABLES
#computing mean doy temperature
dfcarbon_group_doy =dfcarbon_group %>% group_by(doy_numeric) %>% summarize(across(all_of(c("Dinoflagellate","Diatom_noDetritus","Ciliate"))))

ggplot(data=dfcarbon_group,aes(x = doy_numeric,y = (temp_beam))) +
  geom_point(alpha=0.2,color="red")+
  geom_line(data=dfcarbon_group_doy,aes(x=doy_numeric,y=mean_temp),color="black")+
  facet_grid(cols=vars(regime))+
  xlab("Day of Year")+
  ylab("Temperature (Deg C)")+
  theme(text = element_text(size = 15))


ggplot(data=dfcarbon_group,aes(x = month,y = temp_beam)) +
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


ggplot(data=dfcarbon_group,aes(x = date,y = (salinity_beam))) +
  geom_point(color="black")+
  scale_x_date(date_breaks="1 year", date_labels="%Y")+
  xlab("Date")+
  ylab("Salinity (PSU)")

ggplot(data=dfcarbon_group,aes(x = week,y = (salinity_beam))) +
  geom_boxplot(alpha=0.2,color="black")+
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Salinity (PSU)")+
  theme(text = element_text(size = 15))


ggplot(data=dfcarbon_group,aes(x = week,y = (wind_speed))) +
  geom_boxplot(alpha=0.2,color="blue")+
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Wind Speed")+
  theme(text = element_text(size = 15))

ggplot(data=dfcarbon_group,aes(x = week,y = (wind_dir))) +
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


#######################################

dfnut$doy_numeric <- factor(dfnut$doy_numeric)
dfnut$week <- factor(week(dfnut$date))
dfnut$month <- factor(month(dfnut$date))

regime_1_index = (which(dfnut$year < regime_1_end))
regime_2_index = (which((dfnut$year >= regime_1_end)&(dfnut$year < regime_2_end)))
regime_3_index = (which(dfnut$year >= regime_2_end))

dfnut$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfnut$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfnut$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")


ggplot(data=dfnut,aes(x = month,y = nitrate_mean)) +
  geom_boxplot(alpha=0.5) + 
  facet_grid(cols=vars(regime))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+
  xlab("Month of Year")+
  ylab("Nitrate Concentration (umol/L) ")+
  theme(text = element_text(size = 15))


ggsave(filename="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/group_level/nitrate-regime.png")

ggplot(data=dfnut,aes(x = month,y = phosphate_mean)) +
  geom_boxplot(alpha=0.5,color="purple") + 
  facet_grid(cols=vars(regime))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+
  xlab("Month of Year")+
  ylab("Phosphate Concentration  (umol/L)")+
  theme(text = element_text(size = 15))

ggsave(filename="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/group_level/phosphate-regime.png")


ggplot(data=dfnut,aes(x = month,y = silicate_mean)) +
  geom_boxplot(alpha=0.5,color="brown") + 
  facet_grid(cols=vars(regime))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+
  facet_grid(cols=vars(regime))+
  xlab("Month of Year")+
  ylab("Silicate Concentration  (umol/L)")+
  theme(text = element_text(size = 15))

ggsave(filename="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/group_level/silicate-regime.png")


ggplot(data=dfnut,aes(x = month,y = ammonia_mean)) +
  geom_boxplot(alpha=0.5) + 
  facet_grid(cols=vars(regime))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  scale_x_discrete(breaks=seq(1,12,1))+  facet_grid(cols=vars(regime))+
  xlab("Month of Year")+
  ylab("Ammonia Concentration")
ggsave(filename="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/group_level/ammonia-regime.png")


##############################################
ggplot(data=dfnut,aes(x = month,y = nitrate_mean/phosphate_mean)) +
  geom_boxplot(alpha=0.5) + 
  facet_grid(cols=vars(regime))+
  xlab("Month of Year")+
  ylab("N:P Ratio")+
  ylim(0,20)+
  theme(text = element_text(size = 15))

ggsave(filename="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/group_level/np_ratio-regime.png")


########################## SPECIES LEVEL ###########################
dfcarbon_conc$regime = NaN
dfcarbon_conc$year = year(dfconc$date)
dfcarbon_conc$week = factor(week(dfconc$date))
dfcarbon_conc$month = factor(month(dfconc$date))

regime_1_index = (which(dfcarbon_conc$year < regime_1_end))
regime_2_index = (which((dfcarbon_conc$year >= regime_1_end)&(dfcarbon_conc$year < regime_2_end)))
regime_3_index = (which(dfcarbon_conc$year >= regime_2_end))

dfcarbon_conc$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfcarbon_conc$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfcarbon_conc$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")

ggplot(data=dfcarbon_conc,aes(x = month,y = Chaetoceros_danicus)) + 
  geom_boxplot(alpha=0.5) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(sides="l")+
  facet_grid(cols=vars(regime))+
  xlab("Month of Year")+
  ylab("Chaetoceros Danicus Concentration")
ggsave(filename="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/group_level/chaetoceros_danicus-regime.png")

ggplot(data=dfcarbon_conc,aes(x = week,y =Thalassiosira)) +
  geom_boxplot(alpha=0.5) + 
  scale_x_discrete(breaks=seq(1,52,5))+
  facet_grid(cols=vars(regime))+
  ylim(0,10000)+
  xlab("Month of Year")+
  ylab("Thalassiosira Concentration")
ggsave(filename="/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/group_level/thalassiosira-regime.png")



##########################################
#RANK ORDER PLOTS
#generate rank order of top 10 diatoms within each regime
#retrieve IFCB classlist

dfcarbon_conc$year = year(dfcarbon_conc$date)
dfcarbon_conc$month = factor(month(dfcarbon_conc$date))

#creating regime column to sort rows into 3 regimes
dfcarbon_conc$regime = NaN
regime_1_index = (which(dfcarbon_conc$year < regime_1_end))
regime_2_index = (which((dfcarbon_conc$year >= regime_1_end)&(dfcarbon_conc$year < regime_2_end)))
regime_3_index = (which(dfcarbon_conc$year >= regime_2_end))

dfcarbon_conc$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfcarbon_conc$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfcarbon_conc$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")


shared_diatoms <- intersect(diatom_index,colnames(dfcarbon_conc))
df_ranked <- dfcarbon_conc %>% select(all_of(c("regime","month",shared_diatoms))) %>%
  group_by(regime,month) %>% 
  summarize(across(all_of(shared_diatoms),sum))%>%
  pivot_longer(cols=shared_diatoms,names_to=c("Species"),values_to=c("Concentration")) %>%
  group_by(regime,month)%>%
  mutate(rank = rank(-Concentration))%>%
  ungroup()

df_ranked$month <- as.numeric(df_ranked$month)
df_ranked$rank <- as.integer(df_ranked$rank)

ggplot(df_ranked, aes(x = month, y = rank, color = Species)) +
  facet_grid(cols=vars(regime))+
  ylim(5,1)+
  geom_bump(size=1.5)+
  scale_fill_brewer(palette="Dark2")





