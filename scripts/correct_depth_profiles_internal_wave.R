#PURPOSE:
list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp","tidyr","scales","formula.tools","ggpubr","DescTools","gsw")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

remotes::install_github("NEFSC/NEFSC-Spatial") #loads NEFSC Spatial Data
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))

strata_index = mvco_strata

#PLOTTING NUTRIENT PROFILES
mvco_strata = c(21,24,25,20)
gom_basin = c(41,42,37,34)
mvco_offshore = c(19,23)
northeast_channel = c(38,39)
# strata_index = mvco_strata
# strata_name="mvco_nearshore"
# strata_index = mvco_offshore
# strata_name = "MVCO_offshore"
strata_index = gom_basin
strata_name = "GOM_BASIN"

regime_name = "2006 - 2011"
# regime_name = "2012 - 2017"
# regime_name = "2018 - 2022"

#create data frame
temp <- dfj %>% filter(season=="Summer",STRATA %in% strata_index,
                       nitrite_nitrate_flag==2,nitrite_nitrate!=-999,
                       regime != NaN) %>%
  mutate(lat=signif(lat,4),lon=signif(lon,4)) %>%
  group_by(date,lat,lon)%>%
  mutate(profile_id = cur_group_id())


df_density = temp %>% select(salinity,depth_sampling,lon,lat,temp,nitrite_nitrate,profile_id,regime,date) %>% drop_na()

SA <- gsw_SA_from_SP(SP = df_density$salinity,
                     p = df_density$depth_sampling,
                     longitude = df_density$lon,
                     latitude = df_density$lat)

CT <- gsw_CT_from_t(SA,df_density$temp,df_density$depth_sampling)
df_density$density <- gsw_rho(SA,CT,10.1325)-1000



ggplot(df_density) + geom_line(aes(y=nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id))) + 
  geom_point(aes(y=nitrite_nitrate,x=depth_sampling,color=as.factor(profile_id))) + 
  scale_x_reverse()+coord_flip()+
  facet_grid(cols=vars(regime))+
  scale_colour_discrete(name = "Profile ID")+
  xlab(expression("Depth (m)")) + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_depth_profile_regime_RAW_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

ggplot(df_density) + geom_line(aes(y=nitrite_nitrate,x=density,color=as.factor(profile_id))) + 
  geom_point(aes(y=nitrite_nitrate,x=density,color=as.factor(profile_id))) + 
  scale_x_reverse()+coord_flip()+
  facet_grid(cols=vars(regime))+
  scale_colour_discrete(name = "Profile ID")+
  xlab(expression("Density (kg m"^-3*")")) + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  guides(color = FALSE, size = FALSE)
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_depth_profile_density_regime_CORRECTED_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)


density_bins = seq(22,27,0.04)
df_density <- df_density %>% drop_na() %>% arrange(profile_id) %>%
  group_by(profile_id) %>% mutate(count = n_distinct(depth_sampling)) %>%
  filter(count > 1) %>% ungroup()
profile_ids = unique(df_density$profile_id)
sigDepths = matrix(nrow=length(density_bins),ncol=length(profile_ids))
for (ii in 1:length(profile_ids)){
  print(ii)
  a <- df_density %>% filter(profile_id == profile_ids[ii]) %>%
    arrange(desc(depth_sampling))
  sigDepths[,ii] <- approx(a$density,a$depth_sampling,xout=density_bins,ties="mean",na.rm=T)$y
}

colSums(!is.na(sigDepths))

#compute mean isopycnal depth
df_depth=data.frame(mDepth = rowMeans(sigDepths,na.rm=TRUE),density = density_bins)

ggplot(df_depth) + geom_line(aes(x=density,y=mDepth))+coord_flip()+scale_x_reverse()+
  xlab(expression("Density (kg m"^-3*")")) + ylab("Mean Isopycnal Depth (m)")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_max_anova_regime_CORRECTED_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

sigN = matrix(nrow=length(density_bins),ncol=length(profile_ids))
for (ii in 1:length(profile_ids)){
  print(ii)
  a <- df_density %>% filter(profile_id == profile_ids[ii]) %>%
    arrange(desc(density))
  sigN[,ii] <- approx(a$density,a$nitrite_nitrate,xout=density_bins,ties="mean",na.rm=T)$y
}


require(reshape2)

nonNA_sigN=which(colSums(!is.na(sigN)) >=5)
nonNA_sigDepths=which(colSums(!is.na(sigDepths)) >=5)
nonNA_ind = intersect(nonNA_sigN,nonNA_sigDepths)

df_sigN = as.data.frame(sigN)
colnames(df_sigN)= profile_ids
df_sigN = df_sigN[,nonNA_ind]
df_sigN$density_bins <- density_bins
df_sigN$mDepth <- df_depth$mDepth
dfN= melt(df_sigN,id.vars =c("density_bins","mDepth"),variable.name="profile_id",value.name="sigN")

ggplot(dfN) + geom_line(aes(x=mDepth,y=sigN,color=as.factor(profile_id)))+
  # facet_grid(cols=vars(regime))+
  coord_flip()+scale_x_reverse()+
  guides(color = FALSE, size = FALSE)+
   xlab("Mean Isopycnal Depth (m)") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_max_anova_regime_CORRECTED_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)


dfN_merged=merge(df_density[c("date","lat","lon","profile_id","regime")],dfN,by="profile_id")
dfN_merged = drop_na(dfN_merged)


ggplot(dfN_merged) + geom_line(aes(x=mDepth,y=sigN,color=as.factor(profile_id)))+
  facet_grid(cols=vars(regime))+
  coord_flip()+scale_x_reverse()+
  guides(color = FALSE, size = FALSE)+
  xlab("Mean Isopycnal Depth (m)") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_depth_profiles_regime_CORRECTED_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)


dfN_merged_stats <- dfN_merged %>% group_by(profile_id) %>%
  mutate(nut_max = max(sigN))  %>%
  filter(mDepth >= 20,mDepth <=150,sigN >=5) %>% mutate(nit_sum_deep =mean(sigN,na.rm=T))

ggboxplot(data=dfN_merged_stats , x="regime",y="nut_max",color="regime")+
  stat_compare_means(method="anova",label.y=max(df_temp$nut_max)+1)+
  xlab("Period") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_max_anova_regime_CORRECTED_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)

ggboxplot(data=dfN_merged_stats , x="regime",y="nit_sum_deep",color="regime")+
  stat_compare_means(method="anova",label.y=max(dfN_merged_stats$nit_sum_deep)+1)+
  xlab("Period") + ylab(expression("[Nitrite+Nitrate] (umol kg"^-1*")"))+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+ labs(color="Period")+
  grids(linetype = "dashed")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_deep_mean_regime_CORRECTED_",strata_name,".png"),
       width=600,height=500,units="px",dpi=120)
