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
df_carbonC_filled_trimmed <- df_carbonC_filled %>% mutate(year = year(date)) %>% filter(year>=2006,year<=2022)

for(taxa_i in label_maybe_include){
  print(taxa_i)
  ts_taxa <- ts(df_carbonC_filled_trimmed[taxa_i],start=c(2006,1),end=c(2022,366),frequency=366) #53 indicates weekly
  decom_taxa_add <- decompose(ts_taxa,type="additive")
  
  Time = attributes(ts_taxa)[[1]]
  Time = seq(Time[1],Time[2], length.out=(Time[2]-Time[1])*Time[3])
  
  # Convert td to data frame
  dat = cbind(Time, with(decom_taxa_add, data.frame(Observed=x,
                                                    Trend=trend,
                                                    Seasonal=seasonal,
                                                    Random=random)))
  
  df_long <- gather(dat, component, value, -Time)
  df_long$component_f <- factor(x$component,levels=c("Observed","Trend","Seasonal","Random"))
  
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
