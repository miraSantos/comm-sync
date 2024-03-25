basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices",
                      "astsa","dtw")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/df_carbon_2024_Mar_24.RData"))

quadroot <- function(x){x^(1/4)}

doy_means <- df_carbonC %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  group_by(doy_numeric) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

head(doy_means)


alignment<-dtw(doy_means[,full_periodicity_list[1]],doy_means[,full_periodicity_list[2]],keep=TRUE);

## Display the warping curve, i.e. the alignment curve
plot(alignment,type="threeway")

kmeans
