df_carbonC_wyear_mean$year <- as.numeric(df_carbonC_wyear_mean$year)

sum(is.na(df_carbonC_wyear_mean[df_carbonC_wyear_mean$year==2021,"Paralia_sulcata"]))

ref_year_interp %>% filter(year==2021) %>% 
  ggplot() + geom_point(aes(x=date,y=Paralia_sulcata))


df_carbonC_wyear_mean$year <- as.numeric(df_carbonC_wyear_mean$year)
df_carbonC %>% filter(year==2021) %>% 
  ggplot() + geom_point(aes(x=date,y=Paralia_sulcata))

df_carbon$year= year(df_carbon$date)
df_carbon%>% filter(year==2021) %>% 
  ggplot() + geom_point(aes(x=date,y=Paralia_sulcata))+
  
  scale_x_date(date_breaks="1 month",date_labels="%b")

df_carbonC_wyear_mean %>%mutate_at(protist_tricho_labelC,quadroot) %>% filter(year==2019) %>% 
  ggplot() + geom_point(aes(x=date,y=Paralia_sulcata))

df_carbonC_wyear_mean %>%mutate_at(protist_tricho_labelC,quadroot) %>% 
  ggplot() + geom_point(aes(x=date,y=Paralia_sulcata))+
  scale_x_date(date_breaks="1 year",date_labels =format("%Y"))

week_climatology %>% ggplot() + geom_point(aes(x=week,y=Paralia_sulcata))


df_carbonC %>% filter(year==2021) %>% ggplot() + geom_point(aes(x=date,y=Paralia_sulcata))

num_weeks_filled = df_carbonC_wyear_mean %>% group_by(year) %>% summarize(num=n())
######################################################3

df_carbonC_wyear_mean %>% filter(year==2006)

index = df_carbonC_wyear_mean[df_carbonC_wyear_mean$year==2006,]$week
length(week_climatology[index,]$Acantharia)
df_carbonC_wyear_mean[df_carbonC_wyear_mean$year==2006,]$Acanthoica_quattrospina

str(individual_year)
for(y in 1:length(years)){
  print(years[y])
  dtw_distance = rep(NA,times=length(protist_tricho_labelC))
  #extract week year means of a specific year
  individual_year <- df_carbonC_wyear_mean %>% ungroup() %>% filter(year == years[y]) %>% select(protist_tricho_labelC,week) %>%
    mutate_at(protist_tricho_labelC,quadroot) %>% drop_na()
  week_index= individual_year %>% drop_na() %>% select(week)
  climatology = week_climatology[week_index$week,]
  df_y= individual_year[,protist_tricho_labelC]
  #extract diagonal of the correlation matrix
  correlation= diag(cor(climatology,df_y))
  append_this_cor <- as.data.frame(t(c(year=years[y],correlation)))
  #append to dataframe
  df_cor <- rbind(df_cor,append_this_cor)
  annual_peak <- rbind(annual_peak,c(year=years[y],sapply(individual_year, max, na.rm = TRUE)))
}


c_index_median = apply(df_cor[,protist_tricho_labelC],2,median,na.rm=T)
c_index_sd <- apply(df_cor[,protist_tricho_labelC], 2, sd,na.rm=T)


d