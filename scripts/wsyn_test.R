list.of.packages <- c("RColorBrewer","lubridate","fields",
                      "ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","wsyn","tidyr",
                      "moments","plotrix","useful")

#install packages if they aren't already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(list.of.packages, require, character.only = TRUE)

times = 1:200

period = 50
ts = sin(2*pi/period*times)

plot(times,ts)
ts<-cleandat(ts,times,clev=5)

wtres<-wt(ts$cdat,times)
class(wtres)
plotmag(wtres)

ts.pol = cart2pol(times,ts)
ts.pol$theta
polar.plot(ts,times)


transform <- function(x){
  y = ((x - min(x))/(max(x)-min(x)))^(1/3)
  return(y)
}

bio1 = transform(df_freq$Tintinnopsis)
temp = transform(df_freq$temp_beam)

plot(df_freq$doy_numeric,bio1)
points(df_freq$doy_numeric,temp,col="red")
