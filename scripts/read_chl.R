

list.of.packages <- c("lubridate","dplyr","ggplot2","tidyverse","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

chl <- read.csv(paste0(basepath,"data/mvco/mvco_chl/chl_avg.csv"),header=F)
chl_date <- read.csv(paste0(basepath,"data/mvco/mvco_chl/chl_date.csv"),header=F)
chl_depth <- read.csv(paste0(basepath,"data/mvco/mvco_chl/chl_depth.csv"),header=F)
chl_lat <- read.csv(paste0(basepath,"data/mvco/mvco_chl/chl_lat.csv"),header=F)
chl_lon <- read.csv(paste0(basepath,"data/mvco/mvco_chl/chl_lon.csv"),header=F)

head(chl)
head(chl_date)

df_chl <- cbind(chl_date,chl_depth,chl_lat,chl_lon,chl)
colnames(df_chl) <- c("datetime","depth","lat","lon","chl_a","chl_b","chl_c")
head(df_chl)

df_chl$date <- as.Date(df_chl$datetime,format = "%d-%b-%Y %H:%M:%S")

df_chl <- df_chl %>%
  rowwise() %>%
  mutate(chl_mean = mean(c_across(c('chl_a', 'chl_b', 'chl_c')), na.rm=TRUE))

df_chl$year <- year(df_chl$date)

regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(df_chl$year < regime_1_end))
regime_2_index = (which((df_chl$year >= regime_1_end)&(df_chl$year < regime_2_end)))
regime_3_index = (which(df_chl$year >= regime_2_end))

df_chl$regime <- NaN

df_chl$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
df_chl$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
df_chl$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")
df_chl$week <- week(df_chl$date)

save(df_chl,file="/home/mira/MIT-WHOI/github_repos/comm-sync/data/mvco/mvco_chl/df_chl_2023_Nov_13.RData")

head(df_chl)

df_chl %>% filter(depth==0) %>% ggplot() + geom_point(aes(x=as.factor(week),y=chl_mean))+
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week") + ylab("Chl Concentration")

ggsave(filename=paste0(basepath,"/figures/chl/direct_MVCO/mvco_chl_regime.png"))


df_chl %>% filter(depth==0) %>% ggplot() + geom_line(aes(x=date,y=chl_mean))+
  scale_x_date(date_breaks="2 year",date_labels="%Y")+
  xlab("Time (year)") + ylab("Chl concentration")

ggsave(filename=paste0(basepath,"/figures/chl/direct_MVCO/mvco_chl_time_series.png"))

