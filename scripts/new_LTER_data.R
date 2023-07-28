list.of.packages <- c("RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","ggmap","broom","sf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/dos/MIT-WHOI/community_sychrony/scripts/adv_biwavelet_packages.R")
rm(wt) #replaces wt with the correct biwavelet version (there are 2 versions)

#loading in counts and carbon estimates
dfcarbon <- read.csv("/dos/MIT-WHOI/community_sychrony/data/raw/CSVs/carbon_count_NES_LTER.csv",header=TRUE)
mdata <- read.csv("/dos/MIT-WHOI/community_sychrony/data/raw/CSVs/metadata_carbon_NES_LTER.csv",header=TRUE)


head(dfcarbon)
head(mdata)

#selecting for all underway samples and unflagged data
index_cut = (mdata$sample_type=="underway")&(mdata$skip == 0)
mdata = mdata[index_cut,]
dfcarbon = dfcarbon[index_cut,]

shared_list = intersect(all_index,colnames(dfcarbon))

mdata$date =  as.Date(mdata$datetime,format="%d-%b-%Y %H:%M:%S")
dfcarbon$ml_analyzed = mdata$ml_analyzed

dfcarbon$date <- as.Date(mdata$datetime,format="%d-%b-%Y %H:%M:%S")
dfcarbon = dfcarbon %>% mutate(date=floor_date(date)) %>% group_by(date) %>% reframe(across(all_of(shared_list), ~ sum(.)/sum(ml_analyzed)))

ggplot() + geom_point(aes(x = mdata$longitude,y=mdata$latitude,color = dfcarbon$Asterionellopsis_glacialis))

