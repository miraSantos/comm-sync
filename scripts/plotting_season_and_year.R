
save_r_path = "/home/mira/MIT-WHOI/github_repos/comm-sync/data/ifcb/r_objects/unfilled/"
load(paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
load(paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfcarbon_class$date, "%m")]
dfcarbon_class$season = seasons

################################################################################
### PLOTTING ENVIRONMENTAL VARIABLES BY SEASON AND YEAR
################################################################################
ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=AvgWindDir))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Wind Direction (deg)")+
  ggtitle("Summer Wind Direction at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-wind-dir-MVCO.png",
       width = 2000,height=500,units="px",dpi =175)

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=AvgWindSpeed))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ggtitle("Summer Wind Speed at MVCO")+
  ylab("Wind Speed (knots)")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-wind-speed-MVCO.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=Beam_temperature_corrected))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ggtitle("Summer Temperature at MVCO")+
  ylab("Temperature (Deg C)")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\summer-temp-MVCO.png",
       width = 2000,height=500,units="px",dpi =175)

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=salinity_beam))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ggtitle("Salinity at MVCO (Not Corrected)")+
  ylab("Salinity (psu)")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-salinity-MVCO.png",
       width = 2000,height=500,units="px",dpi =175)



################################################################################
#SUMMER-BIO
################################################################################

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=protist_tricho))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylim(0,3e5)+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Summer Protist Carbon Concentrations at MVCO")+
  xlab("Week of Year")

ggsave(filename=paste0(save_fig_path,"func_group\\summer-protist-tricho-yearly.png"),
       width = 2000,height=500,units="px",dpi =175)



ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=Diatom_noDetritus))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ylim(0,1.2e5)+
  ggtitle("Summer Diatom Carbon Concentrations at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\summer-diatom-yearly.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=Dinoflagellate))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Summer Dinoflagellate Carbon Concentrations at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\summer-dino-yearly.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=Ciliate))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Summer Ciliate Carbon Concentrations at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\summer-ciliate-yearly.png",
       width = 2000,height=500,units="px",dpi =175)

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=NanoFlagCocco))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Summer Nanoflagellate and Coccolithophore Carbon Concentrations at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\summer-nanoflagcocco-yearly.png",
       width = 2000,height=500,units="px",dpi =175)

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Summer",], aes(x=week))+
  geom_boxplot(aes(y=Diatom_noDetritus/(Dinoflagellate+Ciliate+NanoFlagCocco)))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Carbon Concentration Ratio")+
  ggtitle("Diatom:(Other Groups) Ratio at MVCO")+
  xlab("Week of Year")


################################################################################
## FALL - BIO
################################################################################
ggplot(data=dfcarbon_group[dfcarbon_group$season=="Fall",], aes(x=week))+
  geom_boxplot(aes(y=protist_tricho))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ylim(0,3e5)+
  ggtitle("Fall Protist Carbon Concentration at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\fall-protist-yearly.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data=dfcarbon_group[dfcarbon_group$season=="Fall",], aes(x=week))+
  geom_boxplot(aes(y=Diatom_noDetritus))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ylim(0,1.2e5)+
  ggtitle("Fall Diatom Carbon Concentration at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\fall-diatom-yearly.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data=dfcarbon_group[dfcarbon_group$season=="Fall",], aes(x=week))+
  geom_boxplot(aes(y=Dinoflagellate))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Fall Dinoflagellate Carbon Concentration at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\fall-dino-yearly.png",
       width = 2000,height=500,units="px",dpi =175)


# FALL - ENV

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Fall",], aes(x=week))+
  geom_boxplot(aes(y=AvgWindSpeed))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Wind Speed (cm/s)")+
  ggtitle("Fall Wind Speed at MVCO")+
  xlab("Week of Year")

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Fall",], aes(x=week))+
  geom_boxplot(aes(y=AvgWindDir))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Wind Direction (deg)")+
  ggtitle("Fall Wind Direction at MVCO")+
  xlab("Week of Year")


ggplot(data=dfcarbon_group[dfcarbon_group$season=="Fall",], aes(x=week))+
  geom_boxplot(aes(y=Beam_temperature_corrected))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Temperature (deg C)")+
  ggtitle("Fall Temperature at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\fall-temperature-yearly.png",
       width = 2000,height=500,units="px",dpi =175)



################################################################################
## SPRING
################################################################################

ggplot(data=df_sal[df_sal$season=="Spring",], aes(x=week))+
  geom_boxplot(aes(y=salt))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(9,23,5))+
  ylab("Salinity (psu))")+
  ggtitle("Corrected Spring Salinity at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\spring-salinity-yearly.png",
       width = 2000,height=500,units="px",dpi =175)

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Spring",], aes(x=week))+
  geom_boxplot(aes(y=protist_tricho))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Spring Protist Carbon Concentration at MVCO")+
  xlab("Week of Year")

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Spring",], aes(x=week))+
  geom_boxplot(aes(y=Diatom_noDetritus))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Spring Diatom Carbon Concentration at MVCO")+
  xlab("Week of Year")

ggplot(data=dfcarbon_group[dfcarbon_group$season=="Spring",], aes(x=week))+
  geom_boxplot(aes(y=Dinoflagellate))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(36,48,5))+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Spring Dinoflagellate Carbon Concentration at MVCO")+
  xlab("Week of Year")


################################################################################
########### SUMMER AND FALL
################################################################################
ggplot(data=dfcarbon_group[(dfcarbon_group$season=="Summer")|(dfcarbon_group$season=="Fall"),], aes(x=week))+
  geom_boxplot(aes(y=protist_tricho))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,48,10))+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("Summer and Fall Protist Carbon Concentration at MVCO")+
  xlab("Week of Year")


