basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/2024-06-04_df_carbonC_filled_super_res_paul.RData"))


#add date time objects
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

seasons = metseasons[format(df_carbonC$date, "%m")]
df_carbonC <- df_carbonC %>% mutate(doy_numeric = yday(date),
                                    week = week(date),year=year(date),
                                    wyear=paste0(year,"-",week),
                                    season=seasons) 


df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

unadjusted_seasonal_mean <- function(df,taxa_i,week_i){
  mean(pull(df[df$week==week_i,],taxa_i),na.rm=T)
}

shift_adjusted_seasonal_mean <- function(df,taxa_i,week_i,shift){
  mean(pull(df[df$week==week_i-shift,],taxa_i),na.rm=T)
}

unadjusted_seasonal_mean(df_carbonC_wyear_mean,protist_tricho_labelC[1],1)
shift_adjusted_seasonal_mean(df_carbonC_wyear_mean,protist_tricho_labelC[1],1,-3%%-53)

a = df_carbonC_wyear_mean[which(df_carbonC_wyear_mean$week==1),taxa_i]
unadjusted_seasonal_mean(df_carbonC,protist_tricho_labelC[1],1)


k_means$week)
taxa_i = protist_tricho_labelC[1]
taxa_i

head(df_carbonC[,c("week",taxa_i)])
range(df_carbonC[,c("year",taxa_i)]$year)
sum(df_carbonC[,c("week",taxa_i)][taxa_i]/16,na.rm=T)
df_carbonC[,c("week",taxa_i)]



