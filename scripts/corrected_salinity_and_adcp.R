
# READING IN SALT,ADCP DATA
datapath = "/home/mira/MIT-WHOI/github_repos/comm-sync/data"
date <- paste0(datapath,"/mvco/mvco_steven_salt/date_mv.csv")
salt <- paste0(datapath,"/mvco/mvco_steven_salt/salt_mv.csv")
adcp <- paste0(datapath,"/mvco/mvco_steven_salt/wtadcp_mv.csv")

date_salt = read.csv(date,header=F)
sal_salt = read.csv(salt,header=F)
adcp_mv = read.csv(adcp,header=F)

#converting to data frame and changing dataframe headers
df_sal = data.frame(date = date_salt,salt = sal_salt,adcp = adcp_mv)
colnames(df_sal) <- c("date","salt","adcp")

#adding date, week, year, and season columns
df_sal$date <- as.Date(df_sal$date, format = "%d-%b-%Y %H:%M:%S")
df_sal$week <- as.factor(week(df_sal$date))
df_sal$year <- year(df_sal$date)


df_sal$regime = NaN
regime_1_end = 2012
regime_2_end = 2018
regime_1_index = (which(df_sal$year < regime_1_end))
regime_2_index = (which((df_sal$year >= regime_1_end)&(df_sal$year < regime_2_end)))
regime_3_index = (which(df_sal$year >= regime_2_end))

df_sal$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
df_sal$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
df_sal$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")



metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")
seasons = metseasons[format(df_sal$date, "%m")]
df_sal$season = seasons

#plotting summer salinity
ggplot(data=df_sal[(df_sal$season=="Summer")&(df_sal$year >=2006),], aes(x=week))+
  geom_boxplot(aes(y=salt))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Salinity (psu)")+
  ggtitle("Summer Salinity at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\summer-salinity.png",
       width = 2000,height=500,units="px",dpi =175)

#plotting summer current flow
ggplot(data=df_sal[(df_sal$season=="Summer")&(df_sal$year >=2006),], aes(x=week))+
  geom_boxplot(aes(y=adcp))+  facet_grid(cols=vars(year))+
  scale_x_discrete(breaks=seq(23,36,5))+
  ylab("Flow")+
  ggtitle("Summer Current Flow at MVCO")+
  xlab("Week of Year")

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\environmental\\summer-adcp.png",
       width = 2000,height=500,units="px",dpi =175)



ggplot(data=df_sal,aes(x = week,y = salt))+
  geom_boxplot(color="darkgreen",outlier.color="black")+ 
  facet_grid(cols=vars(regime))+
  scale_x_discrete(breaks=seq(1,52,5))+
  xlab("Week of Year")+
  ylab("Salinity (psu)")+
  theme(text = element_text(size = 15))+
  ggtitle("Corrected Salinity Measurements at MVCO")

ggplot(data=df_sal) + geom_point(aes(x=date,y=salt),size=1)+
  xlab("Time")+ylab("Salinity (PSU)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")

figuresavepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/"
ggsave(paste0(figuresavepath,"/environmental/salinity/salinity_mvco.png"),
       width=1000,height=500,units="px",dpi=175)


ggplot(data=dfcarbon_group) + geom_point(aes(x=date,y=Beam_temperature_corrected),size=1)+
  xlab("Time")+ylab("Temperature (Deg C)")+
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")

figuresavepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/figures/"
ggsave(paste0(figuresavepath,"/environmental/temp/temperature_mvco.png"),
       width=1000,height=500,units="px",dpi=175)


  
