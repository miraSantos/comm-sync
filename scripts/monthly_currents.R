list.of.packages <- c("lubridate","ggplot2","dplyr","gridExtra","tidyr","RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)



date <- read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\mvco_monthly_steve\\monthly_date.csv",header=F)
windstress_imag <- read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\mvco_monthly_steve\\wind_stress_imag.csv",header = F)
windstress_real <- read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\mvco_monthly_steve\\wind_stress_real.csv",header=F)

current_imag <- read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\mvco_monthly_steve\\current_averaged_imag.csv",header = F)
current_real <- read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\mvco_monthly_steve\\current_averaged_real.csv",header=F)



head(windstress_real)
plot(windstress_real)

df = data.frame(date = date, windstress_imag = windstress_imag, 
                windstress_real = windstress_real,
                current_imag = current_imag, current_real = current_real)
colnames(df) <- c("date","windstress_imag","windstress_real","current_imag","current_real")
head(df)

df$date = as.Date(df$date,format = "%d-%b-%Y")
df$year = year(df$date)
df$month = month(df$date)

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(df$date, "%m")]
df$season = seasons

ggplot(data = df[df$season == "Summer",])+
  geom_line(aes(x = month,y = windstress_imag))+
  # scale_x_discrete(breaks = seq(6,8,1))+
  facet_grid(cols = vars(year))



ggplot(data = df[df$season == "Summer",])+
  geom_line(aes(x = month,y = current_imag))+
  # scale_x_discrete(breaks = seq(6,8,1))+
  facet_grid(cols = vars(year))


ggplot(data = df[df$season == "Summer",])+
  geom_point(aes(x = month,y = windstress_imag,colour=year))
