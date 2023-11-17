

list.of.packages <- c("ggplot2","ncdf4","raster","sf","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


nes_url <- paste0(basepath,"/data/shapefile/lme.shp") #shape file from https://www.marineregions.org/gazetteer.php?p=details&id=8551

nes_shp<- read_sf(nes_url)


basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
chl_path <- paste0(basepath,"data/ecomon_data/OCCCI/CHLOR_A-CCI/")

chl_list <- list.files(chl_path)

df_chl <- data.frame(chl_sum=numeric(),year=numeric(),week=numeric())
chl_index=10
chl <-nc_open(paste0(chl_path,chl_list[chl_index]))
print(chl)

for(chl_index in 10:length(chl_list)){print(chl_index)
  chl <-nc_open(paste0(chl_path,chl_list[chl_index]))
  {
    sink(paste0(basepath,"/data/ecomon_data/OCCCI/",chl_list[chl_index],"_metadata.txt"))
    print(chl)
    sink()
  }
  
  lon <- ncvar_get(chl, "longitude")
  lat <- ncvar_get(chl, "latitude", verbose = F)
  t <- ncvar_get(chl, "time")
  
  chl.array <- ncvar_get(chl, "CHLOR_A_MEAN") #store the data in a 3-dimensional array
  
  lon_min = -71
  lon_max = -69
  lat_min = 40.5
  lat_max = 42
  
  lon_ind = which((lon > lon_min)&(lon<lon_max))
  lat_ind = which((lat> lat_min)&(lat<lat_max))
  
  chl_sum = colSums(chl.array[lon_ind,lat_ind,],dims=2,na.rm=T)
  years = rep(substring(chl_list[chl_index],4,7),length(chl_sum))
  week_num = seq(1,length(chl_sum),1)
  temp_df <- data.frame(chl_sum=chl_sum,year=years,week=week_num)
  df_chl <- rbind(df_chl,temp_df)
  nc_close(chl)
}


df_chl$date <- as.Date(make_datetime(year = as.numeric(df_chl$year)) + weeks(df_chl$week))

write.csv(df_chl,file=paste0(basepath,"/data/chl_summarized_ecomon_weekly.csv"))

regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(df_chl$year < regime_1_end))
regime_2_index = (which((df_chl$year >= regime_1_end)&(df_chl$year < regime_2_end)))
regime_3_index = (which(df_chl$year >= regime_2_end))

df_chl$regime <- NaN

df_chl$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
df_chl$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
df_chl$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")


ggplot(data=df_chl) + geom_point(aes(x=date,y=chl_sum))+
  scale_x_date(date_breaks="2 year",date_labels="%Y")+
  ylab(expression("Chl-a mean (mg m"^-3*")"))+xlab("Time (years)")
ggsave(filename=paste0(basepath,"/figures/environmental/chl/mvco_region_chl_mean.png"),width=1000,height=500,units="px",dpi=120)

ggplot(data=df_chl) + geom_boxplot(aes(x=as.factor(week),y=chl_sum))+
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  ylab(expression("Chl-a mean (mg m"^-3*")"))+xlab("Time (weeks)")
ggsave(filename=paste0(basepath,"/figures/environmental/chl/mvco_region_chl_mean_regime.png"),
       width=1000,height=500,units="px",dpi=100)

chl.slice <- chl.array[,,32]
r <- raster(t(chl.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')

plot(r,zlim=c(0,14),ylim=c(34,46),xlab="Longitude (Deg W)",ylab="Latitude (Deg N)",main="Net Primary Production (gCarbon/m^2/Day)")
rect(lon_min,lat_min,lon_max,lat_max,border="green")

