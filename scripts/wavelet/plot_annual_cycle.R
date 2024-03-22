basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","grDevices",
                      "astsa")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

df_carbonC$doy_numeric <- yday(df_carbonC$date)
df_carbonC$woy_numeric <- yweek(df_carbonC$date)

doy_means <- df_carbonC %>% group_by(doy_numeric) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

i = 3
i = which(protist_tricho_labelC=="Trichodesmium")
print(protist_tricho_labelC[i])

## TIME SERIES
df_carbonC %>% ggplot() + 
  geom_point(aes_string(x="date",y=protist_tricho_labelC[i]))+
  scale_x_date(date_breaks = "3 years",date_labels="%Y")+
  xlab("Date") + ylab(paste(protist_tricho_labelC[i],"Concentration"))


#DOY pattern 
df_carbonC %>% 
  ggplot() + 
  geom_point(aes_string(x="doy_numeric",y=protist_tricho_labelC[i])) + 
  geom_line(data=doy_means,
            aes_string(x="doy_numeric",y=protist_tricho_labelC[i]),
            color="red") + 
  scale_y_sqrt()


df_carbonC %>% ggplot() + 
  geom_point(aes(x=date,y=Trichodesmium^(1/4)))+
  scale_x_date(date_breaks = "3 years",date_labels="%Y")+
  xlab("Date") + ylab(paste(protist_tricho_labelC[i],"Concentration"))

