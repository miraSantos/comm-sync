list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#Purpose: Plot each yearly current profile side by side
path <- "/home/mira/MIT-WHOI/github_repos/comm-sync/data/mvco/mvco_monthly_steve/"

list.files(path)

height <- read.csv(paste0(path,"height_current.csv"),header=F)
mdate <- read.csv(paste0(path,"monthly_date.csv"),header=F)
summer_i <- read.csv(paste0(path,"summer_current_index.csv"),header=F)$V1
winter_i <- read.csv(paste0(path,"winter_current_index.csv"),header=F)$V1

current_real <- t(read.csv(paste0(path,"current_real.csv"),header=F))
current_imag <- t(read.csv(paste0(path,"current_imag.csv"),header=F))

str(current_real)

rownames(current_real) <- height
colnames(current_real) <- mdate$V1
str(current_real)
current_real

plot(current_real[,1],rownames(current_real),xlab="Curent",ylab="height")

matplot(current_real[,summer_i],type="l")
matplot(current_real[,winter_i],type="l")


current_real_1 <- as.data.frame(current_real)
current_real_1["height"] <- t(height)
dfcurrent_real <-tidyr::pivot_longer(as_tibble(current_real_1),cols=-height,names_to="date",values_to="current")
dfcurrent_real$date <- as.Date(dfcurrent_real$date,format="%d-%b-%Y")
dfcurrent_real$week <- week(dfcurrent_real$date)
dfcurrent_real$year <- year(dfcurrent_real$date)
dfcurrent_real$month <- month(dfcurrent_real$date)
metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfcurrent_real$date, "%m")]
dfcurrent_real$season = seasons

head(dfcurrent_real)

dfcurrent_real %>% filter(season=="Summer") %>% ggplot() +
  geom_point(aes(x=height,y=current))+  facet_grid(cols=vars(year))+
  ggtitle("Summer Current Profiles")+
  geom_hline(yintercept=0,color="red")

dfcurrent_real %>% filter(season=="Winter") %>% ggplot() +
  geom_point(aes(x=height,y=current))+  facet_grid(cols=vars(year))+
  ggtitle("Winter Current Profiles")



