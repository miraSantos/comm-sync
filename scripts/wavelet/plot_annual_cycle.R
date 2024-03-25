basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices",
                      "astsa","dtw")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/df_carbon_2024_Mar_24.RData"))

df_carbonC$doy_numeric <- yday(df_carbonC$date)

quadroot <- function(x){x^(1/4)}

doy_means <- df_carbonC %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
 group_by(doy_numeric) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

i = 3
i = which(protist_tricho_labelC=="Strombidium_conicum")
print(protist_tricho_labelC[i])

## TIME SERIES
df_carbonC %>% ggplot() + 
  geom_point(aes_string(x="date",y=protist_tricho_labelC[i]))+
  scale_x_date(date_breaks = "3 years",date_labels="%Y")+
  xlab("Date") + ylab(paste(protist_tricho_labelC[i],"Concentration"))


df_carbonC$week <- week(df_carbonC$date)
df_carbonC$year <- year(df_carbonC$date)

#DOY pattern 
df_carbonC %>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  mutate(doy_numeric=as.factor(doy_numeric)) %>%
  ggplot() + 
  geom_boxplot(aes_string(x="doy_numeric",y=protist_tricho_labelC[i])) + 
  geom_line(data=doy_means,
            aes_string(x="doy_numeric",y=protist_tricho_labelC[i]),
            color="red")

week_means <- df_carbonC %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  group_by(week) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

ind = which(df_annual$annual_duration==5109)

for(ii in df_annual$species[ind]){
  print(ii)
df_carbonC  %>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  mutate(week=as.factor(week)) %>%
  ggplot() + 
  geom_boxplot(aes_string(x="week",y=ii))
  ggsave(filename=paste0(basepath,"figures/wavelet_transforms/annual_cycle/","annual_cycle_",ii,".png"),
         width=800,height=600,units="px",dpi=100)}

ii = "Acantharia"
df_carbonC  %>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  mutate(week=as.factor(week)) %>%
  ggplot() + 
  geom_boxplot(aes_string(x="week",y=ii))

plot(df_carbonC$date,df_carbonC$detritus^(1/4))


