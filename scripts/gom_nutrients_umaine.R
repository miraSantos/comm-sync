list.of.packages <- c("ggplot2","dplyr","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

gom_url <- "http://grampus.umeoce.maine.edu/nutrients/gomregion.txt"
gomdf <- read_tsv(gom_url,col_names=F)
head(gomdf)
headers <- c("month","day", "year","decimal_longitude","decimal_latitude","bottom_depth","sample_depth","temperature","salinity","NO3+NO2","Si(OH)4","PO4","extracted_chlorophyll","PO4_qc","Si(OH)_qc","NO3+NO2_qc")
names(gomdf) <- headers
head(gomdf)
gomdf$date<-as.Date(with(gomdf,paste(year,month,day,sep="-")),"%Y-%m-%d")

gomdf %>% filter(year >= 2006,PO4_qc > 2) %>% ggplot() + geom_point(aes(x=date,y=PO4))