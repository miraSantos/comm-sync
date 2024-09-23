basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

rss_files = list.files(paste0(basepath,"/results/results_temp_shift/shifts/"),full.names=T)
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso",
                      "rlang")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024-06-13_df_carbonC.RData"))
source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))


jj = 2

taxa = protist_tricho_labelC[jj]

load(rss_files[jj])
length(RSS_optim_season$par)
str(RSS_optim_season)
RSS_optim_season$par

df_env <- read.csv(paste0(basepath,"/data/mvco/mvco_daily_2023.csv"))
df_env$date <- as.Date(df_env$days,format ="%d-%b-%Y")
df_env$year <- year(df_env$date)
df_env$week <- week(df_env$date)

#add date time objects
#map months to seasons
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
                                    season=seasons,
                                    syear=paste0(year,"-",season))

df_merged = left_join(df_carbonC, df_env[c("date","Beam_temperature_corrected")],by="date")


df_season_temp <- df_merged %>% group_by(syear) %>%
  reframe(mean_temp = mean(Beam_temperature_corrected,na.rm=T),date=first(date))

#create dataframe to align shifts with corresponding season and year
#shifts_season for season level shifts
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC_filled$year)-1,1)
sgrid <-expand.grid(seasons,years) #create grid of seasons per year
shifts_season <- data.frame(season = sgrid$Var1,year=sgrid$Var2)%>%
  mutate(syear=paste0(year,"-",season))

df_mean_season <- gen_seasonal_mean(par=RSS_optim_season$par,
                                  df=df_merged,taxa=taxa,
                                  shifts=shifts_season,unit_j="syear",fix_t=F)


head(df_mean_season)
#plot new thing

stats_files = list.files(paste0(basepath,"/results/results_slurm/2024_08_22/stats/"),full.names=T)
stats_files
p.values = data.frame(taxa=character(),p.val=numeric(),c.min=numeric(),c.max=numeric())
for(jj in 1:length(protist_tricho_labelC)){
print(jj)
load(stats_files[jj])

#extract taxa name

if(grepl("Data",strsplit(rss_files[jj],"_")[[1]][7])){
  taxa= strsplit(rss_files[jj],"_")[[1]][6]
} else if( grepl("Data",strsplit(rss_files[jj],"_")[[1]][8])){  
  taxa = paste0(strsplit(rss_files[jj],"_")[[1]][6],"_",strsplit(rss_files[jj],"_")[[1]][7])
} else if( grepl("Data",strsplit(rss_files[jj],"_")[[1]][9])){  
  taxa = paste0(strsplit(rss_files[jj],"_")[[1]][6],"_",strsplit(rss_files[jj],"_")[[1]][7],"_",
                strsplit(rss_files[jj],"_")[[1]][8])
} else{
  taxa = paste0(strsplit(rss_files[jj],"_")[[1]][6],"_",
                strsplit(rss_files[jj],"_")[[1]][7],"_",
                strsplit(rss_files[jj],"_")[[1]][8],"_",
                strsplit(rss_files[jj],"_")[[1]][9])
}

taxa
num_replicates= 100
num.greater= length(which(shift.boot.season$t > orig.ts.stat))
###p-val is fraction of bootstrapped numbers that exceed the original
p.val = num.greater/length(shift.boot.season$t)
hist(shift.boot.season$t)
s.error = sqrt((p.val*(1-p.val))/num_replicates)
c.interval = c(p.val-2*s.error,p.val+2*s.error)
c.interval
p.values[jj,] <- c(taxa,p.val,c.interval)
}


p.values %>% filter(taxa %in% label_maybe_include,(p.val <=0.05 | c.min<0.05))

p.values %>% filter(taxa %in% label_maybe_include) %>% 
  mutate(p.val=as.numeric(p.val),
         c.min=as.numeric(c.min),
         c.max = as.numeric(c.max))%>%
  ggplot() + 
  geom_bar(aes(x=reorder(taxa,-p.val),y=p.val), stat="identity")+
  geom_errorbar(aes(x=reorder(taxa,-p.val),
                    y=p.val,ymin=c.min,
                    ymax=c.max),
                color="black", width=.01) +
  coord_flip()+
  ylab("p-values")+xlab("Taxa")+
  # theme(axis.text.y = element_text(colour = color_code))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) + 
  geom_hline(aes(yintercept=0.05),color="red")+
  ylim(0,1)

