
list.of.packages <- c("lubridate","dplyr","tidyr","zoo","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#reading in data
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
syn_url <- "/home/mira/MIT-WHOI/data/2023/MVCO_syn_euk_conc_2023_Mar.csv"
df_syn_euk <- read.csv(syn_url)
df_syn_euk$date <- as.Date(df_syn_euk$Time_UTC,format = "%d-%b-%Y %H:%M:%S")
head(df_syn_euk)

#computing daily mean
df_syn_euk <- df_syn_euk %>% group_by(date) %>% 
  summarise(syn_biovol_perml = mean(sum_syn_biovol_perml,na.rm=T),
            euk_biovol_perml = mean(sum_euk_biovol_perml,na.rm=T)) %>%
  drop_na()

#select columns to compute index for
cols = c("euk_biovol_perml","syn_biovol_perml")

#Compute cyclic index
#############################################################
#create complete week year index (will left join with dataframe later)

df_syn_euk$doy_numeric=yday(df_syn_euk$date)
df_syn_euk$week=week(df_syn_euk$date)
df_syn_euk$year=year(df_syn_euk$date)
df_syn_euk$wyear <- paste0(df_syn_euk$week,"-",df_syn_euk$year)

week_means_syn <- df_syn_euk %>% 
  mutate_at(cols ,quadroot) %>%
  group_by(week) %>%
  summarize_at(cols,mean,na.rm=T)

#compute mean at every week year
df_syn_wyear_mean <-df_syn_euk %>% group_by(wyear) %>%
  mutate_at(cols,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()


week <- seq(1,53,1)
year <- seq(min(df_syn_wyear_mean$year),max(df_syn_wyear_mean$year),1)
week_list <- rep(week,length(year))
year_list <- rep(year,53)[order(rep(year,53))]
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=as.factor(year_list))

df_syn_wyear_mean$year <- as.factor(df_syn_wyear_mean$year)
#approximate annual cycle with interpolation 
ref_year_interp_syn <- df_syn_wyear_mean%>%
  mutate_at(cols,quadroot) %>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate_at(cols,
            list(~na.approx(object=.,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear"))) 


#average weekly annual cycle across entire time series
week_climatology_syn = week_means_syn %>% select(all_of(cols))

#create dataframe to store correlations
df_cor_syn <- as.data.frame(matrix(nrow=0,ncol=length(cols)+1))
names(df_cor_syn) <- c("year",cols)
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length(cols)+1))
names(annual_peak) <- c("year",cols)
#loop through the years and store colreation
for(y in year){
  print(y)
  #extract week year means of a specific year
  individual_year <- ref_year_interp_syn %>% ungroup() %>% filter(year == y) %>% select(cols)
  #extract diagonal of the correlation matrix
  t= diag(cor(week_climatology_syn,individual_year))
  #append to dataframe
  df_cor_syn <- rbind(df_cor_syn,as.data.frame(t(c(year=y,t))))
  annual_peak <- rbind(annual_peak,c(year=y,sapply(individual_year, max, na.rm = TRUE)))
  t=NA
}
names(df_cor_syn) <- c("year",cols)

#take mean of cyclic index
c_index_syn = colMeans(abs((df_cor_syn[cols])))
c_index_sd <- apply(df_cor_syn[,c("euk_biovol_perml","syn_biovol_perml")], 2, sd,na.rm=T)

c_index_syn = data.frame(cyclicity_index = c_index_syn, sd= c_index_sd)
c_index_syn$species <- c("Picoeuks","Synechococcus")
head(c_index_syn)

#plot correlation over time for each species
for(ii in 1:length(cols)){
  print(paste0(ii," of ",length(cols)))
  ggplot(data=df_cor_syn) + geom_line(aes_string(x="year",y=cols[ii]))+
    geom_point(aes(x={{year}},y=cols[ii]))+
    scale_x_continuous(breaks=seq(min(df$year),max(df$year),2))+ylim(-1,1)+
    ylab("Correlation Coefficient") +xlab("Year")+
    ggtitle(cols[ii])
  ggsave(filename=paste0(basepath,"/figures/cyclic_index_syn/correlation_over_time_quadroot/cyclic_correlation_over_time_",cols[ii],".png"),
         width=600,height=500,units="px",dpi=120)
}

#plot annual cycle colored by year 
ggplot(data=df_syn_euk) + geom_point(aes(x=week,y=syn_biovol_perml^(1/4),color=as.factor(year))) + 
  geom_line(aes(x=week,y=syn_biovol_perml^(1/4),color=as.factor(year)))+
  xlab("Week") + ylab(expression("(Biovolume per ml)"^(1/4)))+
  ggtitle("Syn")
ggplot(data=df_syn_euk) + geom_point(aes(x=week,y=euk_biovol_perml^(1/4),color=as.factor(year))) +
  geom_line(aes(x=week,y=euk_biovol_perml^(1/4),color=as.factor(year)))+
  xlab("Week") + ylab(expression("(Biovolume per ml)"^(1/4)))+
  ggtitle("Euks")


