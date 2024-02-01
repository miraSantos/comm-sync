list.of.packages <- c("readxl","fields","ggplot2","sf","broom","dplyr","lubridate","sp","tidyr","scales","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
remotes::install_github("NEFSC/NEFSC-Spatial")
library(NEFSCspatial)

load(paste0(basepath,"/data/ecomon_nutrients_2007_2023.RData"))
load(paste0(basepath,"/data/strata_nutrients.RData"))



strata_index=mvco_strata
strata_name = "mvco"
df_grouped <- dfj %>% filter((season=="Summer"),
               year >=1920,
               nitrite_nitrate!=-999,
               STRATA %in% strata_index)  %>%
  select(season,year,STRATA,nitrite_nitrate,depth_sampling,regime,phosphate,silicate) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,60,10)),include.lowest=TRUE)))

df_grouped_silicate <- dfj %>% filter((season=="Summer"),
                             year >=1920,
                             silicate!=-999,
                             STRATA %in% strata_index)  %>%
  select(season,year,STRATA,silicate,depth_sampling,regime) %>%
  na.omit()%>%
  mutate(depth_sampling_binned = factor(cut(depth_sampling, breaks = c(seq(0,60,10)),include.lowest=TRUE)))


for(depth_x in unique(df_grouped$depth_sampling_binned)){
my_comparisons = list(c("2006 - 2011","2012 - 2017"),c("2012 - 2017","2018 - 2022"))
df = df_grouped %>% filter(depth_sampling_binned == depth_x)
ggboxplot(df,x="regime",y="nitrite_nitrate")+stat_compare_means(comparisons=my_comparisons)
ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/nitrite_nitrate/","significance_nitrite_nitrate_bigger_bin_",strata_name,"_",depth_x,".png"),
       width=800,height=700,units="px",dpi=150)
}

for(depth_x in unique(df_grouped$depth_sampling_binned)){
  my_comparisons = list(c("2006 - 2011","2012 - 2017"),c("2012 - 2017","2018 - 2022"))
  df = df_grouped_silicate %>% filter(depth_sampling_binned == depth_x)
  ggboxplot(df,x="regime",y="silicate")+stat_compare_means(comparisons=my_comparisons)
  ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/silicate/","significance_silicate_",strata_name,"_",depth_x,".png"),
         width=800,height=700,units="px",dpi=150)
}

for(depth_x in unique(df_grouped$depth_sampling_binned)){
  my_comparisons = list(c("2006 - 2011","2012 - 2017"),c("2012 - 2017","2018 - 2022"))
  df = df_grouped %>% filter(depth_sampling_binned == depth_x)
  ggboxplot(df,x="regime",y="phosphate")+stat_compare_means(comparisons=my_comparisons)
  ggsave(filename=paste0(basepath,"/figures/environmental/nutrients/phosphate/","significance_phosphate_",strata_name,"_",depth_x,".png"),
         width=800,height=700,units="px",dpi=150)
}