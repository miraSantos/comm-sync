dfnut_mvco <- read.csv("/home/mira/MIT-WHOI/data/2023/mvco_nut_2023.csv",header=F)
dfnut_mvco_headers <- read.csv("/home/mira/MIT-WHOI/data/2023/mvco_nut_headers_2023.csv",header=F)
colnames(dfnut_mvco) <- dfnut_mvco_headers$V1
head(dfnut_mvco)
dfnut_mvco$nitrite_nitrate_mean = rowMeans(dfnut_mvco[c("nitrite_nitrate_a","nitrite_nitrate_b","nitrite_nitrate_c")],na.rm=T)
dfnut_mvco$nitrite_nitrate_sd = apply(dfnut_mvco[c("nitrite_nitrate_a","nitrite_nitrate_b","nitrite_nitrate_c")], 1, sd, na.rm=TRUE)
dfnut_mvco <- dfnut_mvco %>% mutate(nitrite_nitrate_rsd=nitrite_nitrate_sd/nitrite_nitrate_mean*100)
dfnut_mvco$phosphate_mean = rowMeans(dfnut_mvco[c("phosphate_a","phosphate_b","phosphate_c")],na.rm=T)
dfnut_mvco$ammonia_mean = rowMeans(dfnut_mvco[c("ammonia_a","ammonia_b","ammonia_c")],na.rm=T)
dfnut_mvco$silicate_mean = rowMeans(dfnut_mvco[c("silicate_a","silicate_b","silicate_c")],na.rm=T)

dfnut_mvco <- dfnut_mvco %>% rowwise() %>% mutate(nitrite_nitrate_mean = mean(c(nitrite_nitrate_a,nitrite_nitrate_b,nitrite_nitrate_c),na.rm=T),
                                    nitrite_nitrate_rsd = sd(c(nitrite_nitrate_a,nitrite_nitrate_b,nitrite_nitrate_c),na.rm=T)/mean(c(nitrite_nitrate_a,nitrite_nitrate_b,nitrite_nitrate_c),na.rm=T)*100,
                                    phosphate_mean = mean(c(phosphate_a,phosphate_b,phosphate_c),na.rm=T),
                                    phosphate_rsd = sd(c(phosphate_a,phosphate_b,phosphate_c),na.rm=T)/mean(c(phosphate_a,phosphate_b,phosphate_c),na.rm=T)*100,
                                    ammonia_mean = mean(c(ammonia_a,ammonia_b,ammonia_c),na.rm=T),
                                    ammonia_rsd = sd(c(ammonia_a,ammonia_b,ammonia_c),na.rm=T)/ mean(c(ammonia_a,ammonia_b,ammonia_c),na.rm=T)*100,
                                    silicate_mean = mean(c(silicate_a,silicate_b,silicate_c),na.rm=T),
                                    silicate_rsd = sd(c(silicate_a,silicate_b,silicate_c),na.rm=T)/mean(c(silicate_a,silicate_b,silicate_c),na.rm=T)*100)

ggplot(data=dfnut_mvco) + geom_point(aes(x=date,y=nitrite_nitrate_rsd))+xlab("Date")+ylab("COV")

dfnut_mvco$date = as.Date(dfnut_mvco$Start_Date,format="%Y-%m-%d %H:%M:%S.0")
dfnut_mvco$doy_numeric = yday(dfnut_mvco$date)
dfnut_mvco$year = year(dfnut_mvco$date)

regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(dfnut_mvco$year < regime_1_end))
regime_2_index = (which((dfnut_mvco$year >= regime_1_end)&(dfnut_mvco$year < regime_2_end)))
regime_3_index = (which(dfnut_mvco$year >= regime_2_end))

dfnut_mvco$regime <- NaN

dfnut_mvco$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfnut_mvco$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfnut_mvco$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")


metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfnut_mvco$date, "%m")]
dfnut_mvco$season = seasons

save(dfnut_mvco,file=paste0(basepath,"data/mvco_nutrients_2023.RData"))

