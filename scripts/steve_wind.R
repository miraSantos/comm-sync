list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)



dfcarbon_group$wind_x = dfcarbon_group$AvgWindSpeed*cos(dfcarbon_group$AvgWindDir)
dfcarbon_group$wind_y = dfcarbon_group$AvgWindSpeed*sin(dfcarbon_group$AvgWindDir)


ggplot(dfcarbon_group[(dfcarbon_group$wind_x>0)&(dfcarbon_group$season == "Summer"),])+
  geom_boxplot(aes(x=week,y=wind_x))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ggtitle("Summer Wind X at MVCO")


ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-wind-x-mvco.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(dfcarbon_group[(dfcarbon_group$wind_y>0)&(dfcarbon_group$season == "Summer"),])+
  geom_boxplot(aes(x=week,y=wind_x))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ggtitle("Summer Wind Y at MVCO")


ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-wind-y-mvco.png",
       width = 2000,height=500,units="px",dpi =175)


#### EAST WARD WIND

ggplot(dfcarbon_group[(dfcarbon_group$wind_y<0)&(dfcarbon_group$season == "Summer"),])+
  geom_boxplot(aes(x=week,y=wind_y))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  scale_y_reverse()+
  ylab("negative u wind speed (knots)")+
  ggtitle("Summer Eastward Wind Speed at MVCO")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-eastward-wind-mvco.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(dfcarbon_group[(dfcarbon_group$wind_x>0)&(dfcarbon_group$season == "Summer"),])+
  geom_boxplot(aes(x=week,y=wind_x))+
  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("positive v wind speed (knots)")+
  ggtitle("Summer Northward Wind Speed at MVCO")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-northward-wind-mvco.png",
       width = 2000,height=500,units="px",dpi =175)


