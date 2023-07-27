list.of.packages <- c("ggplot2","remotes","knitr","rmarkdown")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)
lapply(list.of.packages, require, character.only = TRUE)

############### EXPLORING ecomon salinity data

load("C:\\Users\\Miraflor P Santos\\comm-sync\\ecomon_data\\seasonal_oisst_anom.rda")

ggplot(data= seasonal_oisst_anom[(seasonal_oisst_anom$Var=="Summer")&
                                   (seasonal_oisst_anom$EPU=="MAB"),]) +
  geom_point(aes(x = Time, y = Value))+
  ggtitle("Summer SST Anomaly")


