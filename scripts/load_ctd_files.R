list.of.packages <- c("stringr","fields","ggplot2","sf","broom","dplyr","lubridate","sp",
                      "tidyr","scales","formula.tools","ggpubr","DescTools","gsw","grDevices","ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

folder_path <- "/home/mira/MIT-WHOI/Week.2024.02.25-03.02/ctd/"

meta_data_list <- list.files(paste0(folder_path,"metadata"))
data_list <- list.files(paste0(folder_path,"data"))

df_ctd <- data.frame(matrix(ncol = 10, nrow = 0))
x <- c("temp","salinity","phosphate","nitrite_nitrate","silicate","pressure","lat","lon","date","depth_sampling")
colnames(df_ctd) <- x
df_ctd = as.data.frame(sapply(df_ctd, as.numeric))
df_ctd$date = as.Date(df_ctd$date)

for(ii in 1:length(meta_data_list)){
  print(paste(ii, "of", length(meta_data_list)))
  data <- head(read.csv(paste0(folder_path,"data/",data_list[ii]))[-(1:2),],-1) #removed last row and first 2 rows
  meta_data <- read.csv(paste0(folder_path,"metadata/",meta_data_list[ii]),row.names = NULL,skip=1)
  
  meta_data$row.names <- str_squish(meta_data$row.names)
  data$lat <- as.numeric(str_squish(meta_data[meta_data$row.names=="Latitude","X"]))
  data$lon <- as.numeric(str_squish(meta_data[meta_data$row.names=="Longitude","X"]))
  year <-  as.numeric(str_squish(meta_data[meta_data$row.names=="Year","X"]))
  month <-  as.numeric(str_squish(meta_data[meta_data$row.names=="Month","X"]))
  day <-  as.numeric(str_squish(meta_data[meta_data$row.names=="Day","X"]))
  data$date<-as.Date(with(data,paste(year,month,day,sep="-")),"%Y-%m-%d")
  
  data = data %>% rename(any_of(c(temp = "Temperatur",salinity="Salinity",pressure="Pressure",
                                  nitrate="Nitrate",phosphate="Phosphate",silicate="Silicate",depth_sampling="Depth"))) %>%
    select(any_of(x))%>%mutate_if(is.character, as.numeric)
  df_ctd= bind_rows(df_ctd,data)
}

save(df_ctd,file="/home/mira/MIT-WHOI/github_repos/comm-sync/data/df_ctd_2024_Mar.csv")
save(df_ctd,file="/home/mira/MIT-WHOI/github_repos/comm-sync/data/df_ctd_2024_Mar.RData")

head(df_ctd)
df_ctd$year = year(df_ctd$date)

df_ctd %>% filter(year==2006) %>% ggplot() + geom_sf(data=nes_shp)+
  geom_point(aes(x=lon,y=lat,color=temp)) + xlim()

df_ctd %>% filter(year>=2006, year <=2007) %>% ggplot() + geom_point(aes(x=date,y=temp))
