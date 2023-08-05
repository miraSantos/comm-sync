save_r_path = "C:\\Users\\Miraflor P Santos\\comm-sync\\data\\ifcb\\r_objects\\unfilled\\"
load(paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
load(paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","ggbump","purrr","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#creating indices to indicate regimes
dfcarbon_group$regime = NaN

regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfcarbon_group$year < regime_1_end))
regime_2_index = (which((dfcarbon_group$year >= regime_1_end)&(dfcarbon_group$year < regime_2_end)))
regime_3_index = (which(dfcarbon_group$year >= regime_2_end))

dfcarbon_group$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfcarbon_group$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfcarbon_group$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")






#PLOTTING EACH YEAR AS A COLOR BUT WITHIN THE THREE REGIMES
################################################################################
set.seed(2)
n <- 15
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = sample(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))


ggplot(data=dfcarbon_group,aes(x=doy_numeric))+
  geom_spline(aes(y=Diatom_noDetritus,color=factor(year)))+
  facet_grid(cols=vars(regime))+
  ylim(0,1.5e5)+
  scale_color_manual(values=col_vector)+
  xlab("Day of Year")+
  ylab("Carbon Concentration ug/mL")+
  ggtitle("Diatom Carbon Concentration at MVCO")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\diatom-regime-year-color.png",
       width = 1400,height=400,units="px",dpi =175)


ggplot(data=dfcarbon_group,aes(x=doy_numeric))+
  geom_spline(aes(y=protist_tricho,color=factor(year)))+
  facet_grid(cols=vars(regime))+
  ylim(0,1.5e5)+
  scale_color_manual(values=col_vector)+
  xlab("Day of Year")+
  ylab("Carbon Concentration ug/mL")+
  ggtitle("Protist Carbon Concentration at MVCO")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\protist-regime-year-color.png",
       width = 1400,height=400,units="px",dpi =130)

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

r1 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2006 - 2011",],aes(x=doy_numeric))+
  geom_spline(aes(y=protist_tricho,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1.5e5)+
  ggtitle("2006-2011")

r2 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2012 - 2017",],aes(x=doy_numeric))+
  geom_spline(aes(y=protist_tricho,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1.5e5)+
  ggtitle("2012-2017")

  
r3 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2018 - 2022",],aes(x=doy_numeric))+
  geom_spline(aes(y=protist_tricho,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1.5e5)+
  ggtitle("2018-2022")

ggarrange(r1,r2,r3, ncol = 3, nrow = 1)

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\protist-regime-year-color.png",
       width = 2600,height=600,units="px",dpi =175)


r1 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2006 - 2011",],aes(x=doy_numeric))+
  geom_spline(aes(y=Diatom_noDetritus,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1.5e5)+
  ggtitle("2006-2011")

r2 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2012 - 2017",],aes(x=doy_numeric))+
  geom_spline(aes(y=Diatom_noDetritus,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1.5e5)+
  ggtitle("2012-2017")


r3 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2018 - 2022",],aes(x=doy_numeric))+
  geom_spline(aes(y=Diatom_noDetritus,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1.5e5)+
  ggtitle("2018-2022")

ggarrange(r1,r2,r3, ncol = 3, nrow = 1)

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\diatom-regime-year-color.png",
       width = 2600,height=600,units="px",dpi =175)


###########################################################
#DINOFLAGELLATE
r1 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2006 - 2011",],aes(x=doy_numeric))+
  geom_spline(aes(y=Dinoflagellate,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  scale_y_log10(limits=c(100,1e5))+
  annotation_logticks(sides="l")+
  ggtitle("2006-2011")


r2 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2012 - 2017",],aes(x=doy_numeric))+
  geom_spline(aes(y=Dinoflagellate,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  scale_y_log10(limits=c(100,1e5))+
  annotation_logticks(sides="l")+
  ggtitle("2012-2017")

r3 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2018 - 2022",],aes(x=doy_numeric))+
  geom_spline(aes(y=Dinoflagellate,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  scale_y_log10(limits=c(100,1e5))+
  annotation_logticks(sides="l")+
  ggtitle("2018-2022")

ggarrange(r1,r2,r3, ncol = 3, nrow = 1)

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\dinoflagellate-regime-year-color.png",
       width = 2600,height=600,units="px",dpi =175)


##########################
#NANO FLAG COCCO

r1 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2006 - 2011",],aes(x=doy_numeric))+
  geom_spline(aes(y=NanoFlagCocco,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1e5)+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("2006-2011")

r2 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2012 - 2017",],aes(x=doy_numeric))+
  geom_spline(aes(y=NanoFlagCocco,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1e5)+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("2012-2017")


r3 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2018 - 2022",],aes(x=doy_numeric))+
  geom_spline(aes(y=NanoFlagCocco,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,1e5)+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("2018-2022")

ggarrange(r1,r2,r3, ncol = 3, nrow = 1)

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\NanoFlagCocco-regime-year-color.png",
       width = 2600,height=600,units="px",dpi =175)



#CILIATE

r1 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2006 - 2011",],aes(x=doy_numeric))+
  geom_spline(aes(y=Ciliate,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,6e4)+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("2006-2011")

r2 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2012 - 2017",],aes(x=doy_numeric))+
  geom_spline(aes(y=Ciliate,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,6e4)+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("2012-2017")


r3 = ggplot(data=dfcarbon_group[dfcarbon_group$regime == "2018 - 2022",],aes(x=doy_numeric))+
  geom_spline(aes(y=Ciliate,color=factor(year)),size=1)+
  scale_color_manual(values = colorBlindBlack8)+
  ylim(0,6e4)+
  ylab("Carbon Concentration (ug/mL)")+
  ggtitle("2018-2022")

ggarrange(r1,r2,r3, ncol = 3, nrow = 1)

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\func_group\\Ciliate-regime-year-color.png",
       width = 2600,height=600,units="px",dpi =175)


dfcarbon_group %>%
  map(~ggplot(.,aes(color=factor(year)))+
        geom_spline(aes(x=doy_numeric,y=Diatom_noDetritus))+
        facet_grid(cols=vars(regime))+
        ylim(0,1.5e5)+
        scale_color_manual(values=col_vector)+
        xlab("Day of Year")+
        ylab("Carbon Concentration ug/mL"))
