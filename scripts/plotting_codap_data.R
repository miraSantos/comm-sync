

list.of.packages <- c("dplyr","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
codap_url <- "/home/mira/MIT-WHOI/Week.2023.10.15-21/CODAP_NA_v2021.csv"
df_codap <- read.csv(codap_url,skip=2)
codap_header <- names(read.csv(codap_url))
names(df_codap) <- codap_header
str(df_codap)
names(df_codap)

df_codap$date <- as.Date(paste(df_codap$Year_UTC, df_codap$Month_UTC,df_codap$Day_UTC, sep="-"), "%Y-%m-%d")


range(df_codap$Year_UTC)
metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(df_codap$date, "%m")]
df_codap$season = seasons


df_codap %>% filter(Latitude>=33.49,Latitude <=41.19,
                    Longitude>=-71.99, Longitude<=-65.00,
                    Nitrate_and_Nitrite_flag==2,
                    season=="Summer")%>%
  ggplot()+geom_point(aes(x=date,y=Nitrate_and_Nitrite))





