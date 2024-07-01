basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new 
packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-06-27_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-27_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/2024-06-04_df_carbonC_filled_super_res_paul.RData"))

protist_tricho_labelC <- append(protist_tricho_labelC,"random")

#introducing random time series
df_carbonC$random <- runif(length(df_carbonC$Acanthoica_quattrospina),min=0,max=300)

#add date time objects
df_carbonC$doy_numeric <- yday(df_carbonC$date)
df_carbonC$week <- week(df_carbonC$date)

#compute weekly mean
week_means <- df_carbonC %>% 
  group_by(week) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T) %>%
  mutate_at(protist_tricho_labelC,sqrt) %>% select(all_of(protist_tricho_labelC))

df_carbonC$year <- year(df_carbonC$date)
df_carbonC$wyear <- paste0(df_carbonC$
                             week,"-",df_carbonC$year)
#compute mean at every week year

df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()
df_carbonC_wyear_mean$year <- factor(df_carbonC_wyear_mean$year)

###################################################################
#Compute cyclic index
#############################################################
#create complete week year index (will left join with dataframe)
week <- seq(1,53,1)
years <- seq(2006,max(df_carbonC$year)-1,1)
week_list <- rep(week,length(years))
year_list <- rep(years,53)[order(rep(years,53))]
dfweek <- data.frame(wyear=paste0(week_list,"-",year_list),
                     week=week_list,
                     year=as.factor(year_list))

#fill gaps in data with na.approx()
ref_year_interp <- df_carbonC_wyear_mean%>%
  left_join(dfweek,.,by=c("wyear","week","year"))%>%
  group_by(year) %>%
  mutate_at(protist_tricho_labelC,
            list(~na.approx(object=.,x=week,xout=seq(1,53,1),
                            rule=2,ties=mean,method="linear")))


#create dataframe to store correlations
df_cor <- as.data.frame(matrix(nrow=0,ncol=length(protist_tricho_labelC)+1))
names(df_cor) <- c("year",protist_tricho_labelC)
df_max_xcorr <- data.table::copy(df_cor)
df_lag_xcorr <- data.table::copy(df_cor)
annual_peak <- as.data.frame(matrix(NaN,nrow = 0,ncol=length(protist_tricho_labelC)+1))
names(annual_peak) <- c("year",protist_tricho_labelC)
annual_peak_timing <- as.data.frame(matrix(NaN,nrow = 0,ncol=length(protist_tricho_labelC)+1))
names(annual_peak_timing) <- c("year",protist_tricho_labelC)

#loop through the years and store correlation
for(y in 1:length(years)){
  print(years[y])
  #extract week year means of a specific year
  individual_year <- df_carbonC_wyear_mean %>% ungroup() %>%
    mutate_at(protist_tricho_labelC,sqrt) %>%
    filter(year == years[y]) %>% select(protist_tricho_labelC,week) %>% 
    drop_na()
  #retrieve indices of the week to line up with the climatology
  week_index= individual_year %>% drop_na() %>% select(week)
  #separate out climatology and df_y
  climatology = week_climatology[week_index$week,]
  df_y= individual_year[,protist_tricho_labelC]
  
  #compute correlation matrix - extract diagonal of the correlation matrix
  correlation = diag(cor(climatology,df_y))
  #compute cross correlation and 
  compute_lag <- function(x){if(is.na(max(x$acf))==FALSE){x$lag[which.max(x$acf)]}else{NaN}}
  compute_xcorr<-function(x,y){ccf(x,y,na.action=na.pass,pl=FALSE)}
  #compute cross correation
  xcorr = mapply(compute_xcorr,climatology,df_y,SIMPLIFY=FALSE)
  #extract max correlation
  max_xcorr= lapply(xcorr,function(x)max(x$acf))
  #extract lag at max correlation
  lag_xcorr = lapply(xcorr,compute_lag)
  #reformat to fit dataframes
  append_this_cor <- as.data.frame(t(c(year=years[y],correlation)))
  append_this_xcorr <- as.data.frame(do.call(cbind, max_xcorr)) %>% mutate(year=years[y])
  append_this_lag_xcorr <-  as.data.frame(do.call(cbind, lag_xcorr)) %>% mutate(year=years[y])
  
  #append to dataframes
  df_cor <- rbind(df_cor,append_this_cor)
  df_max_xcorr <- rbind(df_max_xcorr, append_this_xcorr)
  df_lag_xcorr <-rbind(df_lag_xcorr, append_this_lag_xcorr)
  
  annual_peak <- rbind(annual_peak,c(year=years[y],sapply(individual_year[,protist_tricho_labelC],
                                                          max, na.rm = TRUE)))
  annual_peak_timing <- rbind(annual_peak_timing,c(year=years[y],sapply(individual_year[,protist_tricho_labelC],
                                                                        which.max)))
}

names(annual_peak) <- c("year",protist_tricho_labelC)
names(annual_peak_timing) <- c("year",protist_tricho_labelC)

#take mean of cyclic index
c_index_mean = apply(df_cor[,protist_tricho_labelC],2,mean,na.rm=T)
c_index_sd <- apply(df_cor[,protist_tricho_labelC], 2, sd,na.rm=T)
c_index_max_xcorr <-apply(df_max_xcorr[,protist_tricho_labelC], 2, mean,na.rm=T)
c_index_max_xcorr_sd <-apply(df_max_xcorr[,protist_tricho_labelC], 2, sd,na.rm=T)
c_index_lag_corr <-apply(df_lag_xcorr[,protist_tricho_labelC], 2, mean,na.rm=T)

consistency.fun <- function(annual_peak){1 - sd(annual_peak - mean(annual_peak))/mean(annual_peak)}
consistency_index <- apply(annual_peak[,protist_tricho_labelC],2,consistency.fun)

c_index = data.frame(metric_c_mean=c_index_mean,
                     c_sd=c_index_sd,
                     consistency = consistency_index,
                     max_xcorr = c_index_max_xcorr,
                     max_xcorr_sd = c_index_max_xcorr_sd,
                     lag_xcorr = c_index_lag_corr)
c_index$taxa <- rownames(c_index)

#add functional group column to cyclic index object
#load ifcb class list file that categories each species in a functional group
func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton",
                    "Metazoan","Synechococcus","Picoeukaryotes")
func_group_labels <- list(diatom_labelC,dino_labelC,ciliate_labelC,nfg_labelC,
                          metazoan_label,c("Synechococcus"),c("Pico_eukaryotes"))
#create column with functional group 
for(func_group in 1:length(func_group_list)){
  reference=func_group_labels[[func_group]]
  c_index[c_index$taxa%in%reference,"func_group"] = func_group_list[func_group]
}

save(c_index,df_cor,df_max_xcorr,df_lag_xcorr,func_group_list,annual_peak,annual_peak_timing,
     file=paste0(basepath,"/data/r_objects/c_index_df_cor_",Sys.Date(),".RData"))


