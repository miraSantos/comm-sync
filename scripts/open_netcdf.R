list.of.packages <- c("ggplot2","remotes","knitr","rmarkdown",
                      "readxl","lubridate","dplyr","ncdf4")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


nc_path = "/home/mira/MIT-WHOI/Week.2023.09.24-30/WCS7210814882749634085.nc"
nc_file <- nc_open(nc_path)
print(nc_file)