list.of.packages <- c("lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#### PURPOSE: LOAD IN SALINITY DATA FROM STEVE, MVCO DATABASE
#STEVE - ENV_TABLE - LATEST


# READING IN SALT,ADCP DATA from Steve Lentz
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
date <- paste0(basepath,"data/mvco/mvco_steven_salt/date_mv.csv")
salt <- paste0(basepath,"data/mvco/mvco_steven_salt/salt_mv.csv")
adcp <- paste0(basepath,"data/mvco/mvco_steven_salt/wtadcp_mv.csv")

date_salt = read.csv(date,header=F)
sal_salt = read.csv(salt,header=F)
adcp_mv = read.csv(adcp,header=F)

df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_env_table_2023.csv"))
#do not use temp_beam use beam_temp_corrected from the separate folder
head(df_env)
df_env$date <- as.Date(df_env$time_local,format="%d-%b-%Y %H:%M:%S")

salt_merged_aggt_2020 <- df_env %>% select(date,salinity_beam) %>% drop_na() %>% 
  filter(date > as.Date("2019-01-01")) %>%
  rename(salinity_mean = salinity_beam)

#READING IN LATEST SALT DATA
salt_2022 <-read.csv(paste0(basepath,"/data/mvco/ASIT/from_nas/asit.ctd_2022.csv"))
salt_2023 <-read.csv(paste0(basepath,"/data/mvco/ASIT/from_nas/asit.ctd_2023.csv"))
salt_2023_beam <-read.csv(paste0(basepath,"/data/mvco/ASIT/from_nas/asit.beam.ctd_2023.csv"))

plot(salt_2023_beam$salinity_mean)

date_salt_conv <- as.Date(date_salt$V1,format="%d-%b-%Y %H:%M:%S")
salt_merged_agg = data.frame(date = date_salt_conv,salinity_mean = sal_salt$V1)

#merging 2022 and 2023
salt_new <- rbind(salt_2022[-1,],salt_2023_beam[-1,])
salt_new$date <- as.Date(salt_new$timestamp,format="%Y-%m-%dT%H:%M:%S")

salt_merged_raw <- rbind(salt_new %>% select(date,salinity_mean),salt_merged_aggt_2020,salt_merged_agg)

salt_merged_agg <- salt_merged_raw %>% mutate_if(is.character, as.numeric) %>% 
  group_by(date)%>% summarise(salt_mean=median(salinity_mean,na.rm=T)) %>% drop_na()

#creating copy with just date and salinity_mean
write.csv(salt_merged_agg,file=paste0(basepath,"/data/salinity_mvco_2003-2023.csv"))

salt_merged_agg$year <- year(salt_merged_agg$date)

salt_merged_agg$week <- as.factor(week(salt_merged_agg$date))
salt_merged_agg$year <- year(salt_merged_agg$date)

salt_merged_agg$regime = NaN
regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(salt_merged_agg$year < regime_1_end))
regime_2_index = (which((salt_merged_agg$year >= regime_1_end)&(salt_merged_agg$year < regime_2_end)))
regime_3_index = (which(salt_merged_agg$year >= regime_2_end))

salt_merged_agg$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
salt_merged_agg$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
salt_merged_agg$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")
seasons = metseasons[format(salt_merged_agg$date, "%m")]
salt_merged_agg$season = seasons

save(salt_merged_agg,file=paste0(basepath,"/data/r_objects/unfilled/salinity_2023_Oct_31.RData"))
     
# Plotting salinity

salt_merged_agg %>% ggplot() + geom_point(aes(x=date,y=salt_mean))+scale_x_date(date_breaks="2 year",date_labels = "%Y")

salt_merged_agg %>% filter(year>2019) %>% ggplot() + geom_point(aes(x=date,y=salt_mean))

#plotting summer salinity
ggplot(data=salt_merged_agg[(salt_merged_agg$season=="Summer")&(salt_merged_agg$year >=2006),], aes(x=week))+
  geom_boxplot(aes(y=salt_mean))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Salinity (psu)")+
  ggtitle("Summer Salinity at MVCO")+
  xlab("Week of Year")

ggsave(filename=paste0(basepath,"/figures/environmental/salinity/summer-salinity-mvco.png"),
       width = 2000,height=500,units="px",dpi =175)
