#compute weekly anomaly from mean
week_means <- df_carbonC %>% 
  group_by(week) %>%
  summarize_at(opt_list_merged,mean,na.rm=T)

str(df_carbonC_wyear_mean)

df_carbonC_wyear_mean %>% group_by(year) %>% summarize(across(any_of(protist_tricho_labelC),~.x - week_means))



week_means
