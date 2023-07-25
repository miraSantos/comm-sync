list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/dos/MIT-WHOI/community_sychrony/scripts/adv_biwavelet_packages.R")
rm(wt) #replaces wt with the correct biwavelet version (there are 2 versions)

#loading in counts and carbon estimates
dfcount_class_raw <- read.csv("/dos/MIT-WHOI/community_sychrony/data/raw/CSVs/classcount_MVCO.csv",header=TRUE)
dfcarbon_class_raw <- read.csv("/dos/MIT-WHOI/community_sychrony/data/raw/CSVs/carbon_count_MVCO.csv",header=TRUE)
mdata_MVCO <- read.csv("/dos/MIT-WHOI/community_sychrony/data/raw/CSVs/meta_data_MVCO.csv",header=TRUE)
dfcarbon_group_raw <- read.csv("/dos/MIT-WHOI/community_sychrony/data/raw/CSVs/carbon_group_MVCO.csv",header=TRUE)


#LOADING IN ENV
env_url = "https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/mvco_env_table.csv"

df_env <- read.csv(env_url,sep = ",",header=T)
df_env$date <- as.Date(df_env$time_local,format="%d-%b-%Y %H:%M:%S")

group_list = c("protist_tricho","metazoan","Diatom_noDetritus","Dinoflagellate","Ciliate","NanoFlagCocco")

mdata_MVCO$date <- as.Date(mdata_MVCO$sample_time,format="%Y-%m-%d %H:%M:%S+00:00")
mdata_MVCO$doy_numeric=yday(mdata_MVCO$date)
skip_flag_index = mdata_MVCO$skip==0
pos_ml_index = mdata_MVCO$ml_analyzed >= 0

#selecting columns that represent live taxa
shared_list = colnames(dfcount_class_raw)[colnames(dfcount_class_raw)!="pid"]
dfconc = dfcount_class_raw[,shared_list]
dfcarbon_conc = dfcarbon_class_raw[,shared_list]
dfcarbon_group = dfcarbon_group_raw[,group_list]

mdata_MVCO = mdata_MVCO[pos_ml_index,]
dfconc=dfconc[pos_ml_index,]
dfcarbon_conc = dfcarbon_conc[pos_ml_index,]
dfcarbon_group = dfcarbon_group[pos_ml_index,]

dfconc$date = mdata_MVCO$date
dfcarbon_conc$date = mdata_MVCO$date 
dfcarbon_group$date = mdata_MVCO$date
dfconc$ml_analyzed = mdata_MVCO$ml_analyzed
dfcarbon_conc$ml_analyzed = mdata_MVCO$ml_analyzed
dfcarbon_group$ml_analyzed = mdata_MVCO$ml_analyzed

#skipping important groups
dfconc = dfconc[skip_flag_index,]
dfcarbon_conc = dfcarbon_conc[skip_flag_index,]
dfcarbon_group = dfcarbon_group[skip_flag_index,]

dfconc = na.omit(dfconc)
dfcarbon_conc = na.omit(dfcarbon_conc)
dfcarbon_group = na.omit(dfcarbon_group)


#Computing daily concentrations
dfconc = dfconc %>% mutate(date=floor_date(date)) %>% group_by(date) %>% reframe(across(all_of(shared_list), ~ sum(.)/sum(ml_analyzed)))
dfcarbon_conc = dfcarbon_conc %>% mutate(date=floor_date(date)) %>% group_by(date) %>% reframe(across(all_of(shared_list), ~ sum(.)/sum(ml_analyzed)))
dfcarbon_group = dfcarbon_group %>% mutate(date=floor_date(date)) %>% group_by(date) %>% reframe(across(group_list, ~ sum(.)/sum(ml_analyzed)))


df_env$doy_numeric=yday(df_env$date)
head(df_env)

df_env=df_env %>% group_by(date) %>%   summarise(across(c(wind_speed,wind_dir,solar,temp_beam,salinity_beam),function(x) mean(x, na.rm=TRUE)),.groups="drop")
head(df_env)

dfconc <- merge(dfconc,df_env,by="date")
dfcarbon_conc <- merge(dfcarbon_conc,df_env,by="date")
dfcarbon_group <- merge(dfcarbon_group,df_env,by="date")
env_index= colnames(df_env)[(colnames(df_env) != "date" & colnames(df_env) != "doy_numeric")]

dfcarbon_conc$date = as.Date(dfcarbon_conc$date)
dfconc$date = as.Date(dfconc$date)
dfcarbon_group$date = as.Date(dfcarbon_group$date)
#fill gaps
fill_gaps <- function(df,index_list){
df_freq <- df %>% 
  #set to daily frequency
  complete(date = seq.Date(min(df$date),max(df$date), by="day")) %>%
  #fill out doy_numeric
  mutate(doy_numeric = yday(date)) %>%
  group_by(doy_numeric)%>%
  #replace nans for living things with yearly mean
  mutate(across(all_of(index_list),~replace_na(.,mean(.,na.rm=T))))
  return(df_freq)
}
dfconc = fill_gaps(dfconc,c(shared_list,env_index))
dfcarbon_conc = fill_gaps(dfcarbon_conc,c(shared_list,env_index))
dfcarbon_group = fill_gaps(dfcarbon_group,c(group_list,env_index))

save(dfcarbon_group,group_list,file="/dos/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_19_dfcarbon_group.RData")
save(dfconc,dfcarbon_conc,shared_list,file="/dos/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_19_dfcount_index.RData")
write.csv(dfconc,file="/dos/MIT-WHOI/community_sychrony/data/dfconc_2023_Jul_19.csv")
write.csv(dfcarbon_conc,file="/dos/MIT-WHOI/community_sychrony/data/dfcarbon_conc_2023_Jul_19.csv")
