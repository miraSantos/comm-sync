#PURPOSE: pool batches and compute r.2 and significance testing
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","pso",
                      "rlang")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024-08-23_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/filled/2024-09-11_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/cyclicity/shift_functions.R"))

#load each file and compute p-value for statistic
stats_files = list.files("/home/mira/MIT-WHOI/github_repos/comm-sync/results/batch/merged_2/",full.names=T)
stats_files

#map months to seasons
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#add seasons and weeks etc to time series
log_zero <- function(x){log10(x+0.1)}
seasons = metseasons[format(df_carbonC_filled_wyear_mean$date, "%m")]

df_carbonC_filled_wyear_mean <- df_carbonC_filled_wyear_mean %>%
  mutate(doy_numeric = yday(date),
         week = week(date),year=year(date),
         wyear=paste0(year,"-",week),
         season=seasons,
         syear=paste0(year,"-",season))

#create dataframe with years and seasons
seasons <- c("DJF","MAM","JJA","SON")
years <- seq(2006,2022,1)

#expand grid of season per year
sgrid <-expand.grid(seasons,years)
shifts_season <- data.frame(season = sgrid$Var1,year=sgrid$Var2,
                            time_lag = 0,amp_lag=0) %>%
  mutate(syear=paste0(year,"-",season))

shifts_season

#compute and extract all p-values, shifts, and correlations
super_res <- list()
super_a <- list()

#create empty named vector to store bootstrapped values and p values
t.vals <- NULL
t.vals[[protist_tricho_labelC]] <- c()

#store p-values
p.vals <- NULL
p.vals[[protist_tricho_labelC]] <- c()

#store r^2 values
shift.res.all <- data.frame(taxa = character(),r.2 = numeric(),amp_var = numeric(),
                            tss=numeric(),rss =numeric())

################################################################################
#loop through stats files
################################################################################
for(ii in 1:length(stats_files)){
  
load(stats_files[ii])

#compile result object into super_res
super_res[[result$taxa_i]] <- result
#pool boostrap values by taxa
t.vals[[result$taxa_i]] <- c(t.vals[[result$taxa_i]],result$shift.ts$t)


## compute r.2
taxa_i = result$taxa_i
print(taxa_i)

df_carbonC_filled_log <- df_carbonC_filled_wyear_mean %>%
  mutate_at(protist_tricho_labelC,log_zero) %>%
  filter(year>=2006,year<=2022)

df_mean_season <- gen_seasonal_mean(par=result$RSS_optim_season$par,
                                    df=df_carbonC_filled_log,taxa=taxa_i,
                                    shifts=shifts_season,unit_j="syear",fix_t=F)

head(df_mean_season)

#compute seasonal mean
a = df_mean_season %>%
  select(all_of(c("syear",taxa_i,"t_shifted","amp_lag",
                  "seasonal_mean","week","year"))) %>%
  distinct()

super_a[[result$taxa_i]] <- a
  
# seasonal residuals
shift.res <-  a[taxa_i] -a$amp_lag-
  a$seasonal_mean

amp_var <- var(a$amp_lag,na.rm=T)
seasonal_var <- var(log10(a$seasonal_mean),na.rm=T)

rss = var(shift.res,na.rm=T)
tss = var(a[[taxa_i]],na.rm=T)

ggplot(data =a) +
  geom_hline(aes(yintercept=0),color="red")+
  geom_point(aes(x=year,y=amp_lag))+
    theme(axis.text.x = element_text(angle = -45, vjust = 0.1, hjust=0.2))+
  scale_x_continuous(breaks = seq(2003,2022,2))+ylim(-3,3)

ggsave(filename=paste0(basepath,"/figures/amp_lags/",taxa_i,"_",Sys.Date(),".png"),
       width = 3300,height = 2000,units = "px", dpi = 300)



#plot seasonal amplitude lags
ggplot(data =a) +
  geom_hline(aes(yintercept=0),color="red")+
  geom_point(aes(x=syear,y=amp_lag))+
  theme(axis.text.x = element_text(angle = -45, vjust = 0.1, hjust=0.2))+
ylim(-3,3)

ggsave(filename=paste0(basepath,"/figures/seasonal_amp_lags/",taxa_i,"_",Sys.Date(),".png"),
       width = 3300,height = 2000,units = "px", dpi = 300)

#compute r^2
r.2 = 1-(rss/tss)

#bind to master dataframe
shift.res.all <-rbind(shift.res.all,
                      list(taxa=taxa_i,
                           r.2=r.2,
                           amp_var = amp_var,
                           tss = tss,rss = rss,
                           seasonal_var = seasonal_var))
}


#compute mean and standard deviation of r.2
mean.r2 <- shift.res.all %>% group_by(taxa) %>% summarise(mean = mean(r.2))


#loop through and compute pvalues and generate histogram of p-values
for(taxa_i in protist_tricho_labelC){
  print(taxa_i)
num_greater = length(which(t.vals[[taxa_i]] > super_res[[taxa_i]]$orig.ts))
num_greater
p.vals[[taxa_i]] = num_greater/length(t.vals[[taxa_i]])
hist(t.vals[[result$taxa_i]])
}

df_pval = data.frame(taxa = names(p.vals),p.val = unlist(p.vals,use.names=F))
df_pval %>% filter(taxa %in% label_maybe_include) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(as.factor(taxa),-p.val),y=p.val),stat="identity")+
  geom_hline(aes(yintercept=0.05),color="red") + 
  coord_flip()



#add functional group categoreis to dataframe

func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton",
                    "Metazoan","Synechococcus","Picoeukaryotes")
func_group_labels <- list(diatom_labelC,dino_label,ciliate_label,nfg_label,
                          metazoan_label,c("Synechococcus"),c("Pico_eukaryotes"))
#create column with functional group 
for(func_group in 1:length(func_group_list)){
  reference=func_group_labels[[func_group]]
  shift.res.all[shift.res.all$taxa%in%reference,"func_group"] = func_group_list[func_group]
  mean.r2[mean.r2$taxa%in%reference,"func_group"] = func_group_list[func_group]
  
}

str(shift.res.all)

load("/home/mira/MIT-WHOI/github_repos/comm-sync/data/r_objects/unfilled/2024-09-11_df_carbonC.RData")


df_carbonC_filled_wyear_mean$week <- week(df_carbonC_filled_wyear_mean$date)
df_var <- df_carbonC_filled_wyear_mean %>%
  group_by(week) %>% 
  summarize_at(protist_tricho_labelC,var,na.rm=T)

str(df_var)

for(taxa in )
df_var %>% ggplot() + 
  geom_point(aes_string(x="week",
                        y = "Synechococcus")) +
  geom_hline(aes(yintercept = median(.data[["Synechococcus"]],na.rm=T)),color="red")


df_var %>% ggplot() +
  geom_histogram(aes_string(x = "Synechococcus"))

df_var %>% ggplot() +
  geom_histogram(aes_string(x = "Leptocylindrus"))

<- which(df_var[["Synechococcus"]] > median(df_var[["Synechococcus"]],na.rm=T))
