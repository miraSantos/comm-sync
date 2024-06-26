basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","rlang","dtw","scales","patchwork",
                      "moments","stats")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"/data/r_objects/filled/2024-06-13_df_carbonC_filled_merged.RData"))

head(df_carbonC_filled$date)
df_carbonC_filled$year
df_carbonC_filled_trimmed <- df_carbonC_filled %>%
                                mutate(year = year(date),week=week(date),
                                       wyear=paste0(year,"_",week)) %>% 
                              filter(year>=2006,year<=2022) %>%
                              group_by(wyear) %>% 
                              mutate_at(protist_tricho_labelC,mean,na.rm=T)%>%
                              distinct(wyear, .keep_all=TRUE) %>% ungroup()

head(df_carbonC_filled_trimmed)
str(df_carbonC_filled_trimmed)
print(taxa_i)
str(df_carbonC_filled_trimmed[taxa_i])
ts_taxa <- ts(df_carbonC_filled_trimmed[,taxa_i]$Pico_eukaryotes,start=c(2006,1),end=c(2022,53),
              frequency=53) #53 indicates weekly
stl_taxa<- stl(ts_taxa,s.window=53)
autoplot(stl_taxa) + scale_x_continuous(breaks=seq(2002,2023,3))+theme_bw()+
  labs(x="Time",y=expression("Temperature ("*degree*"C)"))

qqnorm(stl_taxa$time.series[, "remainder"])
qqline(stl_taxa$time.series[, "remainder"])
str(stl_taxa$time.series[,"remainder"])
shapiro.test(stl_taxa$time.series[, "remainder"])

for(taxa_i in label_maybe_include){
  print(taxa_i)
  decom_taxa_add <- decompose(ts_taxa,type="additive")
  
  Time = attributes(ts_taxa)[[1]]
  Time = seq(Time[1],Time[2], length.out=(Time[2]-Time[1])*Time[3])
  
  # Convert td to data frame
  dat = cbind(Time, with(decom_taxa_add, data.frame(Observed=x,
                                                    Trend=trend,
                                                    Seasonal=seasonal,
                                                    Random=random)))
  
  df_long <- gather(dat, component, value, -Time)
  df_long$component_f <- factor(df_long$component,levels=c("Observed","Trend","Seasonal","Random"))
  
  ggplot(df_long, aes(Time, value)) +
    facet_grid(component_f ~ ., scales="free_y") +
    geom_line() +
    theme_bw() +
    labs(y=paste(taxa_i,"concentration"), x="Year") +
    scale_x_continuous(breaks=seq(2006,2023,2))+
    theme(plot.title=element_text(hjust=0.5))
  ggsave(filename=paste0(basepath,"/figures/decomposition/decomposition_",Sys.Date(),"_",taxa_i,".png"),
         width=2000,height=2000,units="px",dpi=300)
  
}
