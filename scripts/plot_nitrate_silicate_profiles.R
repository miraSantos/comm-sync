#PURPOSE:plot silicate and nitrate profie
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp",
                      "tidyr","scales","formula.tools","ggpubr","DescTools","gsw","grDevices","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

remotes::install_github("NEFSC/NEFSC-Spatial") #loads NEFSC Spatial Data
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))

strata_index = mvco_strata

#REGIONAL STRATA
mvco_strata = c(21,24,25,20)
gom_basin = c(41,42,37,34)
mvco_offshore = c(19,23)
northeast_channel = c(38,39)
gom_nearshore= c(36)


#UNCOMMENT DESIRED OPTIONS
# strata_index = mvco_strata
# strata_name="mvco_nearshore"
# 
strata_index = mvco_offshore
strata_name = "MVCO_offshore"

# strata_index = gom_basin
# strata_name = "GOM_BASIN"

# strata_index = gom_nearshore
# strata_name = "GOM_nearshore"

# regime_name = "2006 - 2011"
# regime_name = "2012 - 2017"
# regime_name = "2018 - 2022"

#df_profiles create dataframe and add profiles column 
df_profiles <- dfj %>% filter((season=="Spring" | season == "Winter"),STRATA %in% strata_index,
                       nitrite_nitrate_flag==2,nitrite_nitrate!=-999,
                       silicate_flag ==2, silicate!=-999,
                       regime != NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id())


#SILCATE - NITRATE
df_profiles %>%
  ggplot() +
  geom_line(aes(y=silicate-nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=silicate-nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id),shape="nitrate")) + 
  geom_hline(yintercept = 0,color="red",linetype="dashed")+
  facet_grid(cols=vars(regime)) + 
  scale_x_reverse()+coord_flip()+
  xlab("Depth (m)") +ylab(expression("[Silicate] - [Nitrite+Nitrate] ("*mu*"mol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)


#SILCATE - NITRATE
df_profiles %>% group_by(profile_id) %>% mutate(sil_nit_diff = silicate - nitrite_nitrate) %>% ungroup() %>%
  ggplot() +
  geom_point(aes(y=silicate-nitrite_nitrate,x=depth_sampling,color=as.factor(year))) + 
  geom_hline(yintercept = 0,color="red",linetype="dashed")+
  scale_x_reverse()+coord_flip()+
  xlab("Depth (m)") +ylab(expression("[Silicate] - [Nitrite+Nitrate] ("*mu*"mol kg"^-1*")"))

#SILICATE AND NITRATE
df_profiles %>%
  ggplot() +
  geom_line(aes(y=nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id),shape="nitrate")) + 
  geom_line(aes(y=silicate,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=silicate,x=depth_sampling,color=as.factor(profile_id),shape="silicate")) + 
  scale_x_reverse()+coord_flip()+
  scale_colour_discrete(name = "Profile ID")+
  xlab("Depth (m)") + ylab(expression("[Nutrient Concentration] ("*mu*"mol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)


#Individual Profiles
df_profiles %>% filter(profile_id == 8 ) %>%
  ggplot() +
  geom_line(aes(y=nitrite_nitrate,x=depth_sampling,color="nitrate")) +
  geom_line(aes(y=silicate,x=depth_sampling,color="silicate")) +
  geom_point(aes(y=nitrite_nitrate,x=depth_sampling,color="nitrate"),size=1) +
  geom_point(aes(y=silicate,x=depth_sampling,color="silicate"),size=1) +
  scale_x_reverse()+coord_flip()


############## GENERATE STATISTICS
 df_temp <- df_profiles %>% group_by(profile_id) %>% mutate(nut_max = max(silicate-nitrite_nitrate))  %>%
  filter(depth_sampling >= 20,silicate - nitrite_nitrate >=0.1) %>%
  mutate(nit_sum_deep =mean(silicate-nitrite_nitrate,na.rm=T))

 #NIT MAX
ggboxplot(data=df_temp,x="regime",y="nut_max",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_max)+1)+
  xlab("Period") + ylab(expression("[Silicate] - [Nitrite+Nitrate] ("*mu*"mol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate-nitrate/silicate-nitrite_max_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

# Statistical comparison of nit deep mean
ggboxplot(data=df_temp,x="regime",y="nit_sum_deep",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nit_sum_deep)+1)+
  xlab("Period") + ylab(expression("[Silicate] - [Nitrite+Nitrate] ("*mu*"mol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")
 
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate-nitrate/silicate-nitrite_deep_mean_anova_regime_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

