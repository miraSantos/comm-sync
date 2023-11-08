list.of.packages <- c("ggplot2","ncdf4","raster","sf","stars","rasterVis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


nes_url <- paste0(basepath,"/data/shapefile/lme.shp") #shape file from https://www.marineregions.org/gazetteer.php?p=details&id=8551

nes_shp<- read_sf(nes_url)


basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
ppd_path <- paste0(basepath,"/data/ecomon_data/OCCCI/PPD-VGPM2/")

ppd_list <- list.files(ppd_path)

df_ppd <- data.frame(ppd_sum=numeric(),year=numeric(),week=numeric())


for(ppd_index in 10:length(ppd_list)){print(ppd_index)
ppd <-nc_open(paste0(ppd_path,ppd_list[ppd_index]))
{
  sink(paste0(basepath,"/data/ecomon_data/OCCCI/",ppd_list[ppd_index],"_metadata.txt"))
  print(ppd)
  sink()
}

lon <- ncvar_get(ppd, "longitude")
lat <- ncvar_get(ppd, "latitude", verbose = F)
t <- ncvar_get(ppd, "time")

ppd.array <- ncvar_get(ppd, "PPD_MEAN") #store the data in a 3-dimensional array

lon_min = -71
lon_max = -69
lat_min = 40.5
lat_max = 42

lon_ind = which((lon > lon_min)&(lon<lon_max))
lat_ind = which((lat> lat_min)&(lat<lat_max))

ppd_sum = colSums(ppd.array[lon_ind,lat_ind,],dims=2,na.rm=T)
years = rep(substring(ppd_list[ppd_index],4,7),length(ppd_sum))
week_num = seq(1,length(ppd_sum),1)
temp_df <- data.frame(ppd_sum=ppd_sum,year=years,week=week_num)
df_ppd <- rbind(df_ppd,temp_df)
nc_close(ppd)
}

df_ppd$date <- as.Date(make_datetime(year = as.numeric(df_ppd$year)) + weeks(df_ppd$week))

write.csv(df_ppd,file=paste0(basepath,"/data/npp_summarized_ecomon_weekly.csv"))

ggplot(data=df_ppd) + geom_point(aes(x=date,y=ppd_sum))+
  scale_x_date(date_breaks="2 year",date_labels="%Y")+
  ylab("Net Primary Production (gCarbon/m^2/Week)")+xlab("Time (years)")
ggsave(filename=paste0(basepath,"/figures/environmental/PPD/mvco_region_ppd_mean.png"),width=1000,height=500,units="px",dpi=120)


ggplot(data=df_ppd) + geom_point(aes(x=week,y=ppd_sum))

regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(df_ppd$year < regime_1_end))
regime_2_index = (which((df_ppd$year >= regime_1_end)&(df_ppd$year < regime_2_end)))
regime_3_index = (which(df_ppd$year >= regime_2_end))

df_ppd$regime <- NaN

df_ppd$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
df_ppd$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
df_ppd$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")



ggplot(data=df_ppd) + geom_boxplot(aes(x=as.factor(week),y=ppd_sum))+
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  ylab("Net Primary Production (gCarbon/m^2/Week)")+xlab("Time (week)")
ggsave(filename=paste0(basepath,"/figures/environmental/PPD/mvco_region_ppd_mean_regime.png"),
       width=1000,height=500,units="px",dpi=120)


ppd.slice <- ppd.array[,,32]
r <- raster(t(ppd.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')

plot(r,zlim=c(0,14),ylim=c(34,46),xlab="Longitude (Deg W)",ylab="Latitude (Deg N)",main="Net Primary Production (gCarbon/m^2/Day)")
rect(lon_min,lat_min,lon_max,lat_max,border="green")

