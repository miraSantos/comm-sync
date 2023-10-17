save_r_path = "/home/mira/MIT-WHOI/github_repos/comm-sync/data/ifcb/r_objects/unfilled/"
load(paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
load(paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

extract_chl_pp <- function(Variable){
  
  df <- ecodata::chl_pp %>% 
    dplyr::filter(stringr::str_detect(Var,Variable)) %>% 
    tidyr::separate(.,Time, into = c("Cat", "Time2"), sep = "_") %>% 
    tidyr::separate(.,Time2, into = c("year", "week"), sep = 4) %>%
    dplyr::mutate(Year = as.numeric(year),week = as.numeric(week),
                  date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u"))
  
  metseasons <- c(
    "01" = "Winter", "02" = "Winter",
    "03" = "Spring", "04" = "Spring", "05" = "Spring",
    "06" = "Summer", "07" = "Summer", "08" = "Summer",
    "09" = "Fall", "10" = "Fall", "11" = "Fall",
    "12" = "Winter")
  
  seasons = metseasons[format(df$date, "%m")]
  df$season = seasons
  
  regime_1_end = 2012
  regime_2_end = 2018
  regime_1_index = (which(df$year < regime_1_end))
  regime_2_index = (which((df$year >= regime_1_end)&(df$year < regime_2_end)))
  regime_3_index = (which(df$year >= regime_2_end))
  
  df$regime = NA
  
  df$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
  df$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
  df$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")
  
  df$doy_numeric = yday(df$date)
  
  return(df)
}

df_chl = extract_chl_pp(Variable = "WEEKLY_CHLOR_A_MEDIAN")

df_pp = extract_chl_pp(Variable = "WEEKLY_PPD_MEDIAN"

head(dfcarbon_group)

merged_df <- merge(dfcarbon_group, df_pp, by=c("year","week"))
merged_df <- drop_na(merged_df)

x = merged_df$date.x
y1 = merged_df$protist_tricho
y2 = merged_df$Value

par(mar=c(5,5,8,8)+0.1, las=1)

plot.new()
plot.window(xlim=range(x), ylim=range(y1))
points(x, y1, col="red", pch=19)
axis(1)
axis(2, col.axis="red")
box()

plot.window(xlim=range(x), ylim=range(y2))
points(x, y2, col="limegreen", pch=19)
axis(4, col.axis="limegreen")

mtext("Protist Concentration", side = 4, las=3, line=3, col="limegreen")
mtext("Primary Production", side = 2, las=3, line=3, col="red")
