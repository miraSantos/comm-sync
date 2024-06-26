basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

df_group <- read.csv(paste0(basepath,"/data/raw_mvco_ifcb/CSVs/MVCO/groupC_opt.csv"))
meta_data <-read.csv(paste0(basepath,"/data/raw_mvco_ifcb/CSVs/MVCO/groupC_opt_meta_data.csv"))
head(df_group)
head(meta_data)
length(df_group$pid)

func_groups <- c("protist_tricho","metazoan","Diatom_noDetritus","Dinoflagellate",
  "Ciliate","Other_phyto","NanoFlagCocco","NanoFlagCocco_gt5","miscProtist_gt5") 

df_group$date <- as.Date(meta_data$sample_time)
df_group$week <- week(df_group$date)
df_group$year <- year(df_group$date)
head(df_group)

quadroot <- function(x){x^(1/4)}

week_means_quadroot <- df_group %>% 
  mutate_at(func_groups,quadroot) %>%
  group_by(week) %>%
  summarize_at(func_groups,mean,na.rm=T)

df_group$year <- year(df_group$date)
df_group$wyear <- paste0(df_group$week,"-",df_group$year)
#compute mean at every week year

df_group_wyear_mean <-df_group %>% group_by(wyear) %>%
  mutate_at(func_groups,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()
df_group_wyear_mean$year <- factor(df_group_wyear_mean$year)


###################################################################
#Compute cyclic index
#############################################################
#create complete week year index (will left join with dataframe later)
week <- seq(1,53,1)
years <- seq(min(df_group$year),max(df_group$year),1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),week=week_list,year=as.factor(year_list))

#approximate annual cycle with interpolation 
ref_year_interp <- df_group_wyear_mean%>%
  mutate_at(func_groups,quadroot) %>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate_at(func_groups,
            list(~na.approx(object=.,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear"))) 

#average weekly annual cycle across entire time series
week_climatology = week_means_quadroot %>% select(all_of(func_groups))

#create dataframe to store correlations
df_cor_group <- as.data.frame(matrix(nrow=0,ncol=length(func_groups)+1))
df_max_xcorr_group <- data.table::copy(df_cor_group)
df_lag_xcorr_group <- data.table::copy(df_cor_group)
df_dtw <- as.data.frame(matrix(nrow=0,ncol=length(func_groups)+1))
names(df_cor_group) <- c("year",func_groups)
names(df_dtw) <- c("year",func_groups)
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length(func_groups)+1))
names(annual_peak) <- c("year",func_groups)

#loop through the years and store correlation
for(y in 1:length(years)){
  print(years[y])
  dtw_distance = rep(NA,times=length(func_groups))
  #extract week year means of a specific year
  individual_year <- ref_year_interp %>% ungroup() %>% filter(year == years[y]) %>% select(func_groups)
  #extract diagonal of the correlation matrix
  correlation = diag(cor(week_climatology,individual_year))
  
  compute_lag <- function(x){if(is.na(max(x$acf))==FALSE){x$lag[which.max(x$acf)]}else{NaN}}
  #compute cross correlation and 
  compute_xcorr<-function(x,y){ccf(x,y,na.action=na.pass,pl=FALSE)}
  #compute cross correation
  xcorr = mapply(compute_xcorr,week_climatology,individual_year,SIMPLIFY=FALSE)
  #extract max correlation
  max_xcorr= lapply(xcorr,function(x)max(x$acf))
  #extract lag at max correlation
  lag_xcorr = lapply(xcorr,compute_lag)

  df_max_xcorr_group <- rbind(df_max_xcorr_group, as.data.frame(do.call(cbind, max_xcorr)))
  df_lag_xcorr_group <-rbind(df_lag_xcorr_group, as.data.frame(do.call(cbind, lag_xcorr)))
  
  for(i in 1:length(func_groups)){
    dtw_distance[i] <- dtw(individual_year[,i],week_climatology[,i])$normalizedDistance
  } 
  append_this_cor <- as.data.frame(t(c(year=years[y],correlation)))
  append_this_dtw <- as.data.frame(t(c(year=years[y],dtw_distance)))
  #append to dataframe
  df_cor_group <- rbind(df_cor_group,append_this_cor)
  df_dtw <- rbind(df_dtw,append_this_dtw)
  annual_peak <- rbind(annual_peak,c(year=years[y],sapply(individual_year, max, na.rm = TRUE)))
}

names(annual_peak) <- c("year",func_groups)
names(df_dtw) <- c("year",func_groups)

x = seq(1,40,1)
noise = rnorm(length(x),0,sd=0.1)
y=sin(x)
z= sin(x) + 1
dtw(y,z)$normalizedDistance

#take mean of cyclic index
c_index_median = apply(df_cor_group[,func_groups],2,median,na.rm=T)
c_index_sd <- apply(df_cor_group[,func_groups], 2, sd,na.rm=T)
c_index_median_dtw = apply(df_dtw[,func_groups],2,median,na.rm=T)
c_index_max_xcorr_group <-apply(df_max_xcorr_group[,func_groups], 2, mean,na.rm=T)
c_index_max_xcorr_sd_group <-apply(df_max_xcorr_group[,func_groups], 2, sd,na.rm=T)
c_index_lag_xcorr_group <-apply(df_lag_xcorr_group[,func_groups], 2, mean,na.rm=T)
consistency.fun <- function(annual_peak){1 - sd(annual_peak - mean(annual_peak))/mean(annual_peak)}
consistency_index <- apply(annual_peak[,func_groups],2,consistency.fun)

c_index_group = data.frame(cyclicity_index=c_index_median,sd=c_index_sd,
                     consistency = consistency_index,
                     dtw = c_index_median_dtw,
                     max_xcorr=c_index_max_xcorr_group,
                     max_xcorr_sd = c_index_max_xcorr_sd_group,
                     lag_xcorr = c_index_lag_xcorr_group)
c_index_group$func_group <- rownames(c_index_group)


save(c_index_group,df_cor_group,file=paste0(basepath,"/data/r_objects/c_index_df_group_",Sys.Date(),".RData"))


#plot cyclicty index across functional groups.
c_index %>% ggplot()+
  geom_bar(aes(x=species,y=cyclicity_index),color="darkgreen",stat="identity") +
  coord_flip()+
  labs(x="Cyclicity Index",y="Taxa")+
  geom_errorbar(aes(x=reorder(species,+cyclicity_index),
                    y=cyclicity_index,ymin=cyclicity_index-sd,
                    ymax=cyclicity_index+sd),
                color="black", width=.01)

head(df_cor_group)
#plot violin plots
df_cor_group %>% select(func_groups,year) %>% gather(key="func_group",value="corr_coef",-c("year")) %>%
  ggplot() + geom_boxplot(aes(x=func_group,y=corr_coef))+coord_flip()+
  ylim(0,1)+
  labs(x="Functional Group",y="Cyclicity Index")

ggsave(filename=paste0(basepath,"/figures/cyclic_index/groupwise_cyclic_index_boxplot_",Sys.Date(),".png"),
       width=600,height=1000,units="px",dpi=150)
