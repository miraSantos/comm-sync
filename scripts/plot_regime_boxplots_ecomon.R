list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp","tidyr","scales","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))

strata_index = mvco_strata
#BOX PLOT DEPTH PROFILE YEARLY
dfj %>% filter((season=="Summer"),
               year >=1920,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>%
  select(season,year,STRATA,nitrite_nitrate,depth_sampling,regime) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,60,5)),include.lowest=TRUE))) %>% 
  ggplot() + geom_boxplot(aes(x=nitrite_nitrate,y=depth_sampling_binned)) + 
  facet_grid(cols=vars(regime))+
  scale_y_discrete(limits=rev)+
  scale_colour_brewer(name="Year range",,palette = "Dark2")+
  xlab(expression("Nitrite and Nitrate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/nitrite_nitrate_profiles_regime_boxplot_",strata_name,".png"),
       width=800,height=700,units="px",dpi=150)

dfj %>%
  filter((season=="Summer"),
         year >=1920,
         silicate!=-999,
         STRATA %in% strata_index)  %>%
  select(season,year,STRATA,silicate,depth_sampling,regime) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,60,5)),include.lowest=TRUE))) %>% 
  ggplot() + geom_boxplot(aes(x=silicate,y=depth_sampling_binned)) +
  facet_grid(cols=vars(regime))+
  scale_y_discrete(limits=rev)+
  scale_colour_brewer(name="Year range",palette = "Dark2")+
  xlab(expression("Silicate (umol kg"^-1*")"))+ylab("Depth (m)")

ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate_profiles_boxplot_regime_",strata_name,".png"),width=800,height=600,units="px",dpi=150)

 dfj %>% filter((season=="Summer"),
               year >=1920,
               phosphate!=-999,
               phosphate <=7.5,
               STRATA %in% strata_index)  %>%
  select(season,year,STRATA,phosphate,depth_sampling,regime) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,60,5)),include.lowest=TRUE))) %>% 
  ggplot() + geom_boxplot(aes(x=phosphate,y=depth_sampling_binned)) + 
  facet_grid(cols=vars(regime))+
  scale_y_discrete(limits=rev)+
  scale_colour_brewer(name="Year range",,palette = "Dark2")+
  xlab(expression("Phosphate (umol kg"^-1*")"))+ylab("Depth (m)")

p <- stat_compare_means()
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate_profiles_yearly_boxplot_colored_MVCO.png"),width=800,height=600,units="px",dpi=150)
