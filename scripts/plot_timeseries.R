list.of.packages <- c("ggplot2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
load(paste0(basepath,"data/ifcb/r_objects/filled/2023_Jul_28_dfcarbon_group.RData"))

ggplot(data = dfcarbon_group) + geom_line(aes(x=date,y=Diatom_noDetritus))+
  xlab("Time")+ylab("Diatom carbon concentration (ug/mL)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
  scale_y_continuous(label = comma , breaks=seq(0,120000,20000))

ggsave(paste0(basepath,"figures/time_series/diatom_timeseries.png"),
       width=1000,height=500,units="px",dpi=150)


ggplot(data = dfcarbon_group) + geom_line(aes(x=date,y=Dinoflagellate))+
  xlab("Time")+ylab("Dinoflagellate carbon concentration (ug/mL)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
  scale_y_continuous(label = comma)+
  ylim(0,50000)

ggsave(paste0(basepath,"figures/time_series/dino_timeseries.png"),
       width=1000,height=500,units="px",dpi=150)


ggplot(data = dfcarbon_group) + geom_line(aes(x=date,y=protist_tricho))+
  xlab("Time")+ylab("Protist carbon concentration (ug/mL)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
  scale_y_continuous(label = comma)

ggsave(paste0(basepath,"figures/time_series/protist_timeseries.png"),
       width=1000,height=500,units="px",dpi=150)


ggplot(data = dfcarbon_group) + geom_line(aes(x=date,y=Ciliate))+
  xlab("Time")+ylab("Ciliate carbon concentration (ug/mL)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
  scale_y_continuous(label = comma)

ggsave(paste0(basepath,"figures/time_series/ciliate_timeseries.png"),
       width=1000,height=500,units="px",dpi=150)
