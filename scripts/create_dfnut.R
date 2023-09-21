dfnut_heads <- read.csv("/home/mira/MIT-WHOI/data/2021/mvco_nutrients_headers.csv",header=F)
dfnut <- read.csv("/home/mira/MIT-WHOI/data/2023/mvco_nut.csv")
colnames(dfnut) <- dfnut_heads$V1
head(dfnut)
dfnut$nitrate_mean = rowMeans(dfnut[c("nitrate_a","nitrate_b","nitrate_c")],na.rm=T)
dfnut$phosphate_mean = rowMeans(dfnut[c("phosphate_a","phosphate_b","phosphate_c")],na.rm=T)
dfnut$ammonia_mean = rowMeans(dfnut[c("ammonia_a","ammonia_b","ammonia_c")],na.rm=T)
dfnut$silicate_mean = rowMeans(dfnut[c("silicate_a","silicate_b","silicate_c")],na.rm=T)

dfnut$date = as.Date(dfnut$Start_Date,format="%Y-%m-%d %H:%M:%S.0")
dfnut$doy_numeric = yday(dfnut$date)
dfnut$year = year(dfnut$date)

save(dfnut,file="/home/mira/MIT-WHOI/Week.2023.08.28-09.03/comm-sync/data/ifcb/r_objects/unfilled/2023_Sep_11_dfnut.RData")
