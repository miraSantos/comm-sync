basepath="/home/mira/MIT-WHOI/github_repos/comm-sync/"

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
load(paste0(basepath,"data/r_objects/filled/2024-08-23_df_carbonC_filled_wyear_mean.RData"))
source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))

#list fies in 
rss_files = list.files(paste0(basepath,"/results/results_slurm/2024-08-24/rss_cor/"),full.names=T)

load(rss_files[1])

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

seasons = metseasons[format(df_carbonC_filled_wyear_mean$date, "%m")]
df_carbonC_filled_wyear_mean <-df_carbonC_filled_wyear_mean %>% mutate(doy_numeric = yday(date),
                            week = week(date),year=year(date),
                            wyear=paste0(year,"-",week),
                            season=seasons,
                            syear=paste0(year,"-",season)) %>%
                            filter(year >= 2006)

#create dataframe to align shifts with corresponding season and year
#shifts_season for season level shifts
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,max(df_carbonC_filled_wyear_mean$year)-1,1)
sgrid <-expand.grid(seasons,years) #create grid of seasons per year
shifts_season <- data.frame(season = sgrid$Var1,year=sgrid$Var2)%>%
  mutate(syear=paste0(year,"-",season))

#function to compute seasonal mean given set of shifts

shift.res.all <- data.frame(taxa = character(),r.2 = numeric())

for(jj in 1:length(rss_files)){
load(rss_files[jj])
print(rss_files[jj])

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

df_mean_season <- gen_seasonal_mean(par=RSS_optim_season$par,
                                    df=df_carbonC_filled_wyear_mean,taxa=taxa,
                                    shifts=shifts_season,unit_j="syear",fix_t=F)

head(df_mean_season)

a = df_mean_season %>% select(c("syear",taxa,"amp_lag","seasonal_mean")) %>% distinct()

shift.res <-  a[[taxa]] - a$amp_lag -
          a$seasonal_mean

r.2 = 1-(var(shift.res,na.rm=T)/var(a[[taxa]],na.rm=T))

shift.res.all <-rbind(shift.res.all,list(taxa=taxa,r.2=r.2))

for(year_i in years){
  print(year_i)
df_mean_season %>% select(week,seasonal_mean) %>% distinct() %>%
  ggplot() + 
  geom_point(aes(x = week, y = log_zero(seasonal_mean))) +
  geom_point(data = df_carbonC_filled_wyear_mean[df_carbonC_filled_wyear_mean$year==year_i,],aes_string(
    x = "week" , y = taxa ),color="red") +
  scale_x_continuous(breaks=seq(0,53,4)) + ylab("log10 of Seasonal Mean")
  
  ggsave(filename=paste0(basepath,"/figures/yearly_comparison/",taxa,"/",taxa,"_",year_i,".png"),
         width = 1000,height=800,dpi = 150,units= "px")
}
}

str(shift.res.all)
head(shift.res.all)

func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton",
                    "Metazoan","Synechococcus","Picoeukaryotes")
func_group_labels <- list(diatom_labelC,dino_label,ciliate_label,nfg_label,
                          metazoan_label,c("Synechococcus"),c("Pico_eukaryotes"))
#create column with functional group 
for(func_group in 1:length(func_group_list)){
  reference=func_group_labels[[func_group]]
  shift.res.all[shift.res.all$taxa%in%reference,"func_group"] = func_group_list[func_group]
}

###############################################################################
# Plotting ----------------------------------------------------------------
#############################################################################


#plotting the r2 value
my_colors <- RColorBrewer::brewer.pal(7, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
color_code = left_join(shift.res.all[order(shift.res.all$r.2,shift.res.all$func_group),],
                       map,by="func_group",relationship = "many-to-many")$colors
map_dict <- map$colors
names(map_dict) <- map$func_group


shift.res.all %>% filter(taxa %in% label_maybe_include) %>% arrange(r.2) %>% ggplot() +
  geom_bar(aes(x=reorder(taxa,+r.2),y=r.2,fill=func_group),
           stat="identity")+ylim(0,1)+
  coord_flip() + labs(x = "Taxa",y=expression("R"^2))+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(linewidth= 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white")
  )

ggsave(filename=paste0(basepath,
                       "/figures/cyclic_index/r2_",Sys.Date(),".png"),
       width=2000,height=3300,units="px",dpi=300)



shift.res.all %>% filter(taxa %in% label_maybe_include) %>% arrange(r.2) %>% ggplot() +
  geom_bar(aes(x=reorder(taxa,-r.2),y=sqrt(r.2),fill=func_group),
           stat="identity")+ylim(0,1)+
   labs(x = "Taxa",y=expression("R"^2))+
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
  axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
plot.margin=unit(c(0,0,0,1),units="cm"))


ggsave(filename=paste0(basepath,
                       "/figures/cyclic_index/r2_",Sys.Date(),"_horizontal.png"),
       width=4300,height=2000,units="px",dpi=300)


shift.res.all %>% 
  filter(taxa %in% label_maybe_include,
         func_group %in% c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton"))%>%
  ggplot() +
  geom_histogram(aes(x=sqrt(r.2),
                     y=after_stat(count/ave(count, PANEL, FUN = sum))),
                 color="white",bins=15,alpha=0.5)+
  xlim(-0.1,1)+  
  facet_grid(rows=vars(as.factor(func_group)))+
  labs(color  = "Functional\nGroup", fill = "Functional\nGroup",
       x = expression("R"^2), y ="Percent (%)")+
  theme_bw()+
  scale_y_continuous(labels = scales::percent)
################################################################################
#
#################################################################################
jj = 102
load(rss_files[jj])
print(rss_files[jj])

if(grepl("Data",strsplit(rss_files[jj],"_")[[1]][7])){
  taxa= strsplit(rss_files[jj],"_")[[1]][6]
    } else if( grepl("Data",strsplit(rss_files[jj],"_")[[1]][8])){  
            taxa = paste0(strsplit(rss_files[jj],"_")[[1]][6],"_",strsplit(rss_files[jj],"_")[[1]][7])
      } else if( grepl("Data",strsplit(rss_files[jj],"_")[[1]][9])){  
            taxa = paste0(strsplit(rss_files[jj],"_")[[1]][6],"_",strsplit(rss_files[jj],"_")[[1]][7],"_",
                strsplit(rss_files[jj],"_")[[1]][8])
    } else {
  taxa = paste0(strsplit(rss_files[jj],"_")[[1]][6],"_",
                strsplit(rss_files[jj],"_")[[1]][7],"_",
                strsplit(rss_files[jj],"_")[[1]][8],"_",
                strsplit(rss_files[jj],"_")[[1]][9])
}

taxa

df_mean_season <- gen_seasonal_mean(par=RSS_optim_season$par,
                                    df=df_carbonC_filled_wyear_mean,taxa=taxa,
                                    shifts=shifts_season,unit_j="syear",fix_t=F)

head(df_mean_season)

a = df_mean_season %>% select(c("syear",taxa,"amp_lag","seasonal_mean")) %>% distinct()

shift.res <-  a[[taxa]] - a$amp_lag -
  a$seasonal_mean

r.2 = 1-(var(shift.res)/var(a[[taxa]]))

shift.res.all <-rbind(shift.res.all,list(taxa=taxa,r.2=r.2))
df_mean_season

#plot
df_mean_season %>% select(c("week","seasonal_mean",taxa,"year","mean_temp")) %>%
  filter(year==2009) %>% distinct() %>%
  ggplot() + 
  geom_point(aes(x = week, y = seasonal_mean),shape=2) +
  geom_point(aes_string( x = "week", y =taxa),color="green") +
  # geom_point(aes( x = week , y =mean_temp),color="red") +
  scale_x_continuous(breaks=seq(0,53,4)) + ylab("log10 of Seasonal Mean")

df_carbonC_wyear_mean %>% filter(year==2008)%>% ggplot()+
  geom_point(aes(x=week,y=Beam_temperature_corrected),color="red")

load(paste0(basepath,"data/r_objects/c_index_df_cor_2024-06-27.RData"))


index_merged <-left_join(shift.res.all,c_index,by=c("taxa","func_group"))

ggplot(data=index_merged) + 
  geom_abline(slope=1,intercept=0,color="red")+
  geom_point(aes(x=r.2,y=metric_c_mean)) +
  xlim(0,1) + ylim(0,1)+
  xlab(expression("R"^2)) +
  ylab(expression("Cyclicity Index"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"))

ggsave(filename=paste0(basepath,"/figures/c_index_r2_comparison_",Sys.Date(),".png"),
       width =1000,height=1000,units = "px",dpi =200)
