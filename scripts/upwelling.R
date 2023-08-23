
list.of.packages <- c("lubridate","ggplot2","dplyr","gridExtra","tidyr","RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

date <- read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\wind_stress_date.csv",header = F)
stress_imag<- read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\wind_stress_im.csv",header = F)
stress_real <- read.csv("C:\\Users\\Miraflor P Santos\\comm-sync\\data\\mvco\\wind_stress_real.csv",header=F)

#converting to data frame and changing dataframe headers
df_wind = data.frame(date = date$V1,stress_imag=stress_imag, stress_real= stress_real)
colnames(df_wind) <- c("date","stress_imag","stress_real")

#adding date, week, year, and season columns
df_wind$date <- as.Date(df_wind$date, format = "%d-%m-%Y")


df_wind = df_wind %>%
  mutate(date = floor_date(date)) %>%
  group_by(date) %>%
  summarize(mean_stress_imag = mean(stress_imag),mean_stress_real=mean(stress_real))%>%
  ungroup()

df_wind$week <- as.factor(week(df_wind$date))
df_wind$year <- year(df_wind$date)

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(df_wind$date, "%m")]
df_wind$season = seasons
df_wind$season_numeric = NaN
df_wind$season_numeric[df_wind$season == "Winter"] = 1
df_wind$season_numeric[df_wind$season == "Spring"] = 2
df_wind$season_numeric[df_wind$season == "Summer"] = 3
df_wind$season_numeric[df_wind$season == "Fall"] = 4


year_list = unique(df_wind$year[df_wind$year>2005])
num_years = length(year_list)
ui_list =  rep(NaN,num_years) 
num_days_list = rep(NaN,num_years)


###compute upwelling index
latitude = 41.3620 #degree N
v_ang = 7.26e-5# degrees per second
coriolis = 2*v_ang*sin( latitude * ( pi / 180 )) 
density = 1023.6 #kg/m3

# EASTWARD current

for (x in 1:num_years) {

yearly_summer = which((df_wind$season=="Summer")&(df_wind$year==year_list[x])& (df_wind$mean_stress_real>0))

num_days = length(yearly_summer)
ui_list[x] = sum(df_wind$mean_stress_real[yearly_summer]/(density*coriolis*num_days))
num_days_list[x] = num_days
}
df_ui = data.frame(year = year_list,cui = ui_list,num_days = num_days_list)

ggplot(data =df_ui)+geom_point(aes(x=year,y=cui),size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  ggtitle("Summer Eastward Normalized Cummulative Upwelling Index")+
  ylab("CUI (m^2/s)")+
  xlab("Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-eastward-upwelling.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data = df_ui) + geom_point(aes(x = year_list,y=num_days),color="red",size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  xlab("Year")+
  ylab("Number of Days with Data")


ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-upwelling_days_avail.png",
       width = 2000,height=500,units="px",dpi =175)


#SOUTHWARD
for (x in 1:num_years) {
  
  yearly_summer = which((df_wind$season=="Summer")&(df_wind$year==year_list[x])& (df_wind$mean_stress_imag<0))
  

  
  num_days = length(yearly_summer)
  ui_list[x] = sum(df_wind$mean_stress_imag[yearly_summer]/(density*coriolis*num_days))
  num_days_list[x] = num_days
}
df_ui = data.frame(year = year_list,cui = ui_list,num_days = num_days_list)

ggplot(data =df_ui)+geom_point(aes(x=year,y=cui),size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  ggtitle("Summer Southward Cummulative Upwelling Index")+
  ylab("CUI (m^2/s)")+
  xlab("Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-southward-upwelling.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data = df_ui) + geom_point(aes(x = year_list,y=num_days),color="red",size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  xlab("Year")+
  ylab("Number of Days with Data")+
  ggtitle("Number of Days with Available Data for Southward CUI")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-southward-upwelling-data-avail.png",
       width = 2000,height=500,units="px",dpi =175)


#NORTHWARD and SOUTHWARD
for (x in 1:num_years) {
  
  yearly_summer = which((df_wind$season=="Summer")&(df_wind$year==year_list[x]))
  
  
  
  num_days = length(yearly_summer)
  ui_list[x] = sum(df_wind$mean_stress_imag[yearly_summer]/(density*coriolis*num_days))
  num_days_list[x] = num_days
}
df_ui = data.frame(year = year_list,cui = ui_list,num_days = num_days_list)

ggplot(data =df_ui)+geom_point(aes(x=year,y=cui),size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  ggtitle("Net Summer Cummulative Upwelling Index (Northward and Southward)")+
  ylab("CUI (m^2/s)")+
  xlab("Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-net-north-south-upwelling.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data = df_ui) + geom_point(aes(x = year_list,y=num_days),color="red",size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  xlab("Year")+
  ylab("Number of Days with Data")


for (x in 1:num_years) {

yearly_summer = which((df_wind$season=="Summer")&(df_wind$year==year_list[x])& (df_wind$mean_stress_real>0))

num_days = length(yearly_summer)
ui_list[x] = sum(df_wind$mean_stress_real[yearly_summer]/(density*coriolis*num_days))
num_days_list[x] = num_days
}
df_ui = data.frame(year = year_list,cui = ui_list,num_days = num_days_list)

ggplot(data =df_ui)+geom_point(aes(x=year,y=cui),size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  ggtitle("Summer Eastward Normalized Cummulative Upwelling Index")+
  ylab("CUI (m^2/s)")+
  xlab("Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-eastward-upwelling.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data = df_ui) + geom_point(aes(x = year_list,y=num_days),color="red",size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  xlab("Year")+
  ylab("Number of Days with Data")


ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-upwelling_days_avail.png",
       width = 2000,height=500,units="px",dpi =175)

#EASTWARD AND WESTWARD

for (x in 1:num_years) {
  
  yearly_summer = which((df_wind$season=="Summer")&(df_wind$year==year_list[x]))
  
  num_days = length(yearly_summer)
  ui_list[x] = sum(df_wind$mean_stress_real[yearly_summer]/(density*coriolis*num_days))
  num_days_list[x] = num_days
}
df_ui = data.frame(year = year_list,cui = ui_list,num_days = num_days_list)

ggplot(data =df_ui)+geom_point(aes(x=year,y=cui),size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  ggtitle("Net Summer Cummulative Upwelling Index (Eastward and Westward)")+
  ylab("CUI (m^2/s)")+
  xlab("Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-net-east-west-upwelling.png",
       width = 2000,height=500,units="px",dpi =175)


ggplot(data = df_ui) + geom_point(aes(x = year_list,y=num_days),color="red",size=3)+
  scale_x_continuous(breaks = seq(2006,2018,1))+
  xlab("Year")+
  ylab("Number of Days with Data")


ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-upwelling_days_avail.png",
       width = 2000,height=500,units="px",dpi =175)



#########################################################
## EAST - WEST UPWELLING
year_list = unique(df_wind$year)
year_list = year_list[year_list > 2005]
num_years = length(year_list)
num_days = 92

cui_list = matrix(NaN,nrow=num_days,ncol=num_years)
colnames(cui_list) <- year_list

for (year in 1:num_years) {
  
  yearly_summer = which((df_wind$season=="Summer")&(df_wind$year==year_list[year]))

  for (day in 1:num_days){
  
  ui_list[year] = sum(df_wind$mean_stress_real[yearly_summer]/(density*coriolis*num_days))
  cui_list[day,year] = sum(df_wind$mean_stress_real[yearly_summer[1:day]]/(density*coriolis)) 
  }
}

df_wind$doy_numeric = yday(df_wind$date)
cui_list_doy <- cbind(cui_list,df_wind$doy_numeric[yearly_summer])

cui_long <- gather(as.data.frame(cui_list_doy),year,cui,"2006":"2018",factor_key=TRUE)
str(cui_long)
colnames(cui_long) <- c("doy_numeric","year","cui")


n <- 14
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector = rainbow(14)
ggplot(cui_long) + geom_line(aes(x=doy_numeric,y=cui,color=year),size=2)+
  scale_color_manual(values=col_vector)+
  xlab("Day of Year")+
  ylab("CUI (m^2/s)")+
  ggtitle("Summer CUI (Eastward and Westward")


ggplot(cui_long) + geom_line(aes(x=doy_numeric,y=cui),size=2)+
  facet_grid(cols=vars(year))+
  # scale_x_discrete(breaks=seq(152,243,20))+
  xlab("Day of Year")+
  ylab("CUI (m^2/s)")+
  ggtitle("Summer CUI (Eastward and Westward)")


ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-yearly-east-west-upwelling.png",
       width = 2300,height=500,units="px",dpi =175)


#########################################################
## NORTH - SOUTH UPWELLING
year_list = unique(df_wind$year)
year_list = year_list[year_list > 2005]
num_years = length(year_list)
num_days = 92

cui_list = matrix(NaN,nrow=num_days,ncol=num_years)
colnames(cui_list) <- year_list

for (year in 1:num_years) {
  
  yearly_summer = which((df_wind$season=="Summer")&(df_wind$year==year_list[year]))
  
  for (day in 1:num_days){
    
    ui_list[year] = sum(df_wind$mean_stress_imag[yearly_summer]/(density*coriolis*num_days))
    cui_list[day,year] = sum(df_wind$mean_stress_imag[yearly_summer[1:day]]/(density*coriolis)) 
  }
}

df_wind$doy_numeric = yday(df_wind$date)
cui_list_doy <- cbind(cui_list,df_wind$doy_numeric[yearly_summer])

cui_long <- gather(as.data.frame(cui_list_doy),year,cui,"2006":"2018",factor_key=TRUE)
str(cui_long)
colnames(cui_long) <- c("doy_numeric","year","cui")


n <- 14
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector = rainbow(14)
ggplot(cui_long) + geom_line(aes(x=doy_numeric,y=cui,color=year),size=2)+
  scale_color_manual(values=col_vector)+
  xlab("Day of Year")+
  ylab("CUI (m^2/s)")+
  ggtitle("Summer CUI (Eastward and Westward)")


ggplot(cui_long) + geom_line(aes(x=doy_numeric,y=cui),size=2)+
  facet_grid(cols=vars(year))+
  # scale_x_discrete(breaks=seq(152,243,20))+
  xlab("Day of Year")+
  ylab("CUI (m^2/s)")+
  ggtitle("Summer CUI (Northward and Southward)")


ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\wind\\summer-yearly-north-south-upwelling.png",
       width = 2300,height=500,units="px",dpi =175)

  