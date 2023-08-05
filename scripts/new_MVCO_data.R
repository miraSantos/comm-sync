list.of.packages <- c("RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


data_path = "C:\\Users\\Miraflor P Santos\\comm-sync\\data\\ifcb\\csv\\"

#loading in counts and carbon estimates
dfcount_class_raw <- read.csv(paste0(data_path,"count_class_MVCO.csv"),header=TRUE)
dfcarbon_class_raw <- read.csv(paste0(data_path,"carbon_class_MVCO.csv"),header=TRUE)
mdata_MVCO <- read.csv(paste0(data_path,"metadata_MVCO.csv"),header=TRUE)
dfcarbon_group_raw <- read.csv(paste0(data_path,"carbon_group_MVCO.csv"),header=TRUE)


#LOADING IN ENV
env_url = "C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\mvco_env.csv"

df_env <- read.csv(env_url,sep = ",",header=T)
df_env$date <- as.Date(df_env$days,format="%d-%b-%Y")

shared_list = colnames(dfcarbon_class_raw)[colnames(dfcarbon_class_raw)!="pid"]

dfconc_class = dfcount_class_raw[,shared_list]
dfcarbon_class = dfcarbon_class_raw[,shared_list]
dfcarbon_group = dfcarbon_group_raw[,group_list]

mdata_MVCO$date <- as.Date(mdata_MVCO$sample_time,format="%Y-%m-%d %H:%M:%S+00:00")

dfconc_class$date = mdata_MVCO$date
dfcarbon_class$date = mdata_MVCO$date 
dfcarbon_group$date = mdata_MVCO$date

dfconc_class$ml_analyzed = mdata_MVCO$ml_analyzed
dfcarbon_class$ml_analyzed = mdata_MVCO$ml_analyzed
dfcarbon_group$ml_analyzed = mdata_MVCO$ml_analyzed

skip_flag_index = which(mdata_MVCO$skip==0)

#skipping flagged_data
dfconc_class = dfconc_class[skip_flag_index,]
dfcarbon_class = dfcarbon_class[skip_flag_index,]
dfcarbon_group = dfcarbon_group[skip_flag_index,]

# 
# dfconc_class = na.omit(dfconc_class)
# dfcarbon_class = na.omit(dfcarbon_class)
# dfcarbon_group = na.omit(dfcarbon_group)

###########################################################################################

group_list = c("protist_tricho","metazoan","Diatom_noDetritus","Dinoflagellate","Ciliate","NanoFlagCocco")

#Computing daily concentrations
dfconc_class = dfconc_class %>% mutate(date=floor_date(date)) %>% group_by(date) %>% reframe(across(all_of(shared_list), ~ sum(.)/sum(ml_analyzed)))
dfcarbon_class = dfcarbon_class %>% mutate(date=floor_date(date)) %>% group_by(date) %>% reframe(across(all_of(shared_list), ~ sum(.)/sum(ml_analyzed)))
dfcarbon_group = dfcarbon_group %>% mutate(date=floor_date(date)) %>% group_by(date) %>% reframe(across(all_of(group_list), ~ sum(.)/sum(ml_analyzed)))


df_env$doy_numeric=yday(df_env$date)
head(df_env)

df_env = df_env %>% group_by(date) %>% summarise(across(c(AvgSolar,Beam_temperature_corrected,AvgWindDir,AvgWindSpeed),function(x) mean(x, na.rm=TRUE)),.groups="drop")
head(df_env)

dfconc_class <- merge(dfconc_class,df_env,by="date")
dfcarbon_class <- merge(dfcarbon_class,df_env,by="date")
dfcarbon_group <- merge(dfcarbon_group,df_env,by="date")

env_index= colnames(df_env)[(colnames(df_env) != "date" & colnames(df_env) != "doy_numeric")]

dfcarbon_class$date = as.Date(dfcarbon_class$date)
dfconc_class$date = as.Date(dfconc_class$date)
dfcarbon_group$date = as.Date(dfcarbon_group$date)

dfcarbon_class$year = year(dfcarbon_class$date)
dfcarbon_class$week =factor(week(dfcarbon_class$date))
dfcarbon_class$month = factor(month(dfcarbon_class$date))
dfcarbon_class$doy_numeric = yday(dfcarbon_class$date)

dfcarbon_group$year = year(dfcarbon_group$date)
dfcarbon_group$week =factor(week(dfcarbon_group$date))
dfcarbon_group$month = factor(month(dfcarbon_group$date))
dfcarbon_group$doy_numeric = yday(dfcarbon_group$date)


metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfcarbon_group$date, "%m")]
dfcarbon_group$season = seasons

seasons = metseasons[format(dfcarbon_class$date, "%m")]
dfcarbon_class$season = seasons

save_r_path = "C:\\Users\\Miraflor P Santos\\comm-sync\\data\\ifcb\\r_objects\\unfilled\\"
save(dfcarbon_group,group_list,file=paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
save(dfconc_class,dfcarbon_class,shared_list,file=paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))
write.csv(dfconc_class,file=paste0(save_r_path,"dfconc_class_2023_Jul_28.csv"))
write.csv(dfcarbon_class,file=paste0(save_r_path,"dfcarbon_class_2023_Jul_28.csv"))

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

dfconc_class = fill_gaps(dfconc_class,c(shared_list,env_index))
dfcarbon_class = fill_gaps(dfcarbon_class,c(shared_list,env_index))
dfcarbon_group = fill_gaps(dfcarbon_group,c(group_list,env_index))

save_r_path = "C:\\Users\\Miraflor P Santos\\comm-sync\\data\\ifcb\\r_objects\\filled\\"
save(dfcarbon_group,group_list,file=paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
save(dfconc_class,dfcarbon_class,shared_list,file=paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))
write.csv(dfconc_class,file=paste0(save_r_path,"dfconc_class_2023_Jul_28.csv"))
write.csv(dfcarbon_class,file=paste0(save_r_path,"dfcarbon_class_2023_Jul_28.csv"))

