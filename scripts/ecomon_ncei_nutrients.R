list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


nut_data_path = "/home/mira/MIT-WHOI/Week.2023.10.08-14/316G20120202_DE1202_hy1.csv"
nut_data_header <- names(read.csv(nut_data_path,skip=32))
nut_data <- head(read.csv(nut_data_path,skip=34,header=F),-1)
names(nut_data) <- nut_data_header


nut_data$date <- as.Date(as.character(nut_data$DATE),format = "%Y%m%d")

nut_data %>% filter(NITRAT_FLAG_W == 2) %>% ggplot() + geom_point(aes(x=date,y=NITRAT))

nut_data %>% filter(NITRAT_FLAG_W == 2) %>% ggplot() + geom_point(aes(x=date,y=DEPTH))