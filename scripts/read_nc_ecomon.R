list.of.packages <- c("ggplot2","ncdf4","raster","sf","rgdal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


nes_url <- paste0(basepath,"/data/shapefile/lme.shp") #shape file from https://www.marineregions.org/gazetteer.php?p=details&id=8551

nes_shp<- read_sf(nes_url)


basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
ppd_path <- paste0(basepath,"/data/ecomon_data/PPD-VGPM2/")

ppd_list <- list.files(ppd_path)

ppd <-nc_open(ppd_path)

{
  sink(paste0(basepath,"/data/ecomon_data/WW_202201_202252-OCCCI-V6.0-NESGRID4-PPD-VGPM2-STACKED_STATS_metadata.txt"))
  print(ppd)
  sink()
}

lon <- ncvar_get(ppd, "longitude")
lat <- ncvar_get(ppd, "latitude", verbose = F)
t <- ncvar_get(ppd, "time")


ndvi.array <- ncvar_get(ppd, "PPD_MEAN") # store the data in a 3-dimensional array
ndvi.slice <- ndvi.array[,,4]
r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)                                                                                           



ggplot() + geom_raster(data=r,aes(x=x,y=y))
