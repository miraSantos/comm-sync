

# station e01 2015 - 2018
nitrate_gom <- "http://www.neracoos.org/erddap/tabledap/E01_corrected_nitrate_csv.csv?station%2Ctime%2CYear%2CMonth%2CDay%2CHour%2CMinute%2CSecond%2CNitrate_umol%2CStandard_Deviation%2CNitrate%2Clatitude%2Clongitude%2Cmooring_site_desc%2Cdepth%2Cwater_depth&time%3E=2006-11-19T00%3A00%3A00Z&time%3C=2022-11-26T12%3A45%3A00Z"



df <- read.csv(url(nitrate_gom))[-1,]
#reset index
rownames(df) <- NULL

df$date <- as.Date(df$time,format="%Y-%m-%dT%H:%M:%SZ")
df$year <- year(df$date)
df$doy_numeric <- yday(df$date)
df$week <- week(df$date)
df$depth = as.numeric(df$depth)
df$Nitrate = as.numeric(df$Nitrate)

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(df$date, "%m")]
df$season = seasons

head(df)

df %>%
  # filter(season=="Summer")%>% 
  ggplot() +
  geom_boxplot(aes(x=as.factor(week),y=Nitrate))+
  facet_grid(cols=vars(year))+
  ylab("Nitrate (mol m^-3)")+
  xlab("Week of Year")+
  scale_x_discrete(breaks=seq(0,52,10))+
  ggtitle("Nitrate Concentrations at E01 Gulf of Maine")
    
