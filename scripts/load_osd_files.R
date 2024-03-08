library(dplyr)
folder_path <- "/home/mira/MIT-WHOI/Week.2024.02.25-03.02/osd/"

meta_data_list <- list.files(paste0(folder_path,"metadata"))
data_list <- list.files(paste0(folder_path,"data"))

df_osd <- data.frame(matrix(ncol = 10, nrow = 0))
x <- c("temp","salinity","phosphate","nitrite_nitrate","silicate","pressure","lat","lon","date","depth_sampling")
colnames(df_osd) <- x
df_osd = as.data.frame(sapply(df_osd, as.numeric))
df_osd$date = as.Date(df_osd$date)

for(ii in 1:length(meta_data_list)){
  print(paste0(ii, " of ",length(meta_data_list)))
  data <- head(read.csv(paste0(folder_path,"data/",data_list[ii]))[-(1:2),],-1) #removed last row and first 2 rows
  meta_data <- read.csv(paste0(folder_path,"metadata/",meta_data_list[ii]),skip=1)
  
  data$lat <- as.numeric(meta_data["Latitude","X"])
  data$lon <- as.numeric(meta_data["Longitude","X"])
  year <- as.numeric(meta_data["Year","X"])
  month <- as.numeric(meta_data["Month","X"])
  day <- as.numeric(meta_data["Day","X"])
  time <- as.numeric(meta_data["Time","X"])
  data$date<-as.Date(with(data,paste(year,month,day,sep="-")),"%Y-%m-%d")
  
  data = data %>% rename(any_of(c(temp = "Temperatur",salinity="Salinity",pressure="Pressure",
                                  nitrite_nitrate="Nitrate",phosphate="Phosphate",silicate="Silicate",depth_sampling="Depth"))) %>%
    select(any_of(x))%>%mutate_if(is.character, as.numeric)
  data$pressure
  df_osd= bind_rows(df_osd,data)
}

df_osd$temp_flag= 2
df_osd$salinity_flag = 2
df_osd$nitrite_nitrate_flag = 2
df_osd$phosphate_flag = 2
df_osd$silicate_flag = 2

head(df_osd)
save(df_osd,file="/home/mira/MIT-WHOI/github_repos/comm-sync/data/df_osd_2024_Mar.csv")
save(df_osd,file="/home/mira/MIT-WHOI/github_repos/comm-sync/data/df_osd_2024_Mar.RData")

df_osd$year <- year(df_osd$date)

#map of nitrate in 2007
df_osd %>% filter(!is.na(nitrite_nitrate),year==2007) %>% ggplot() + geom_sf(data=nes_shp)+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))

#map of nitrate in 2012
df_osd %>% filter(!is.na(nitrite_nitrate),year==2012) %>% ggplot() + geom_sf(data=nes_shp)+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))

#map of nitrate in 2021
df_osd %>% filter(!is.na(nitrite_nitrate),year==2021) %>% ggplot() + geom_sf(data=nes_shp)+
  geom_point(aes(x=lon,y=lat,color=nitrite_nitrate))

#nitrite_nitrate data over time
df_osd %>% filter(!is.na(nitrite_nitrate)) %>% ggplot() +
  geom_point(aes(x=date,y=nitrite_nitrate)) + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

#phosphate over time
df_osd %>% filter(!is.na(phosphate)) %>% ggplot() +
  geom_point(aes(x=date,y=phosphate)) + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

