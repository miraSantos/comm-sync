save_r_path = "/home/mira/MIT-WHOI/github_repos/comm-sync/data/ifcb/r_objects/unfilled/"
load(paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
load(paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfcarbon_class$date, "%m")]
dfcarbon_class$season = seasons

dfsal %>% ggplot() + geom_point(aes(x=date,y=sa))