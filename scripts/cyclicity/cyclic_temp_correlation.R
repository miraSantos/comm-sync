#PURPOSE: analyze temperature abundance relationship yearly per yearly

basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_26_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_26_df_carbonC.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
load(paste0(basepath,"/data/r_objects/c_index_df_cor_2024_May_13.RData"))
load(paste0(basepath,"/data/r_objects/df_env_2024-05-28.RData"))

df_env_merge <-merge(df_carbonC,df_env[,c("Beam_temperature_corrected","date")], by="date")
df_env_merge$year <- year(df_env_merge$date)
df_env_merge$month <- month(df_env_merge$date)

df_env_merge$wyear <- paste0(week(df_env_merge$date),"_",df_env_merge$year)
df_merge_wyear_mean <-df_env_merge %>%
  group_by(wyear) %>%
  mutate_at(c(label_maybe_include,"Beam_temperature_corrected"),mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()


years = seq(2006,2022,1)
taxa = "Leptocylindrus"
label_maybe_include

#Plot temp vs. carbon conc for each year for each species
for(taxa in label_maybe_include){
  print(taxa)
  for(y in years){
    print(y)
    df_env_merge %>% filter(year==y) %>% ggplot() + 
      geom_density_2d_filled(aes_string(x="Beam_temperature_corrected",y=taxa),bins=5)+
      ggtitle(paste(y,taxa))+scale_y_log10()
    ggsave(filename=paste0(basepath,"/figures/temp_conc_correlation/individual_years/",taxa,"/",taxa,"_",y,".png"),
           width=800,height=600,units="px",dpi=150)
  }
}

taxa="Paralia_sulcata"
taxa="Guinardia_delicatula_merged"
### plot individual 

df_env_merge %>% ggplot() +
  stat_density_2d(aes_string(x="Beam_temperature_corrected",y=taxa,
                             color="year",fill="year"),geom="polygon",alpha=0.4)+
  scale_y_log10(limits=c(0.01,3007465))+ggtitle(taxa)

taxa = "Tripos_merged"
df_merge_wyear_mean %>% filter(year!=2023) %>% mutate(month=as.factor(month)) %>% 
  mutate_at(label_maybe_include,log10) %>%
  ggplot() +
    geom_point(aes_string(x="Beam_temperature_corrected",
                               y=taxa,color="month"))+
    geom_smooth(aes_string(x="Beam_temperature_corrected",y=taxa),
                method="gam",color="black")+
    labs(y=expression("Log10-Transformed Carbon Concentration ("*mu*"g mL"^-1*")"),
         x=expression("Temperature ("*degree*"C)")) + ggtitle(taxa)

taxa = "Acantharia"
df_env_merge %>% ggplot() +
  stat_density_2d(aes_string(x="Beam_temperature_corrected",y=taxa,
                             color="year",fill="year"),geom="polygon",alpha=0.4)+
  scale_y_log10(limits=c(0.01,3007465))+ggtitle(taxa)


head(df_env_merge)
cov(df_env_merge$Beam_temperature_corrected,df_env_merge$Acantharia,na.rm=T)


#############################################################################################
#GENERATE PLOTS IN A LOOP
##############################################################################################
rainbow_mod = c("#0080FF","#0000FF","#8000FF","#FF00FF","#FF0080","#FF0000","#FF8000","#FFFF00","#80FF00","#00FF00","#00FF80","#00FFFF")

#plot all years as acontour shapes for each taxa
for(taxa in label_maybe_include){
  print(taxa)
  df_env_merge %>% ggplot() +
    stat_density_2d(aes_string(x="Beam_temperature_corrected",y=taxa,
                               color="year",fill="year"),geom="polygon",alpha=0.4)+
    scale_y_log10(limits=c(0.1,3007465))+ggtitle(taxa)
  ggsave(filename=paste0(basepath,"/figures/temp_conc_correlation/year_colored/",taxa,".png"),
         width=800,height=800,units="px",dpi=150)
}
#plot all years together as a contour shape for each species
for(taxa in label_maybe_include){
  print(taxa)
  df_env_merge %>% ggplot() +
    stat_density_2d(aes_string(x="Beam_temperature_corrected",y=taxa),geom="polygon",alpha=0.4)+
    scale_y_log10(limits=c(0.1,3007465))+ggtitle(taxa)
  ggsave(filename=paste0(basepath,"/figures/temp_conc_correlation/all_years_combined/",taxa,".png"),
         width=800,height=800,units="px",dpi=150)
}

#plot all years together as a contour shape for each species
for(taxa in label_maybe_include){
  print(taxa)
  df_env_merge %>% ggplot() +
    geom_point(aes_string(x="Beam_temperature_corrected",y=taxa),alpha=0.4)+
    scale_y_log10(limits=c(0.1,3007465))+ggtitle(taxa)
  ggsave(filename=paste0(basepath,"/figures/temp_conc_correlation/all_years_combined/",taxa,".png"),
         width=800,height=800,units="px",dpi=150)
}

taxa="Acantharia"

#plot month in color
for(taxa in label_maybe_include){
  print(taxa)
  df_merge_wyear_mean %>% ggplot(aes(color=as.factor(month))) +
    geom_point(aes_string(x="Beam_temperature_corrected",y=taxa))+
    labs(y=expression("Carbon Concentration ("*mu*"g mL"^-1*")"),x=expression("Temperature ("*degree*"C)"))+
    scale_y_log10(limits=c(0.1,3007465))+ggtitle(taxa)+guides(color=guide_legend(title="Month"))+
    scale_color_manual(values=rainbow_mod)
  ggsave(filename=paste0(basepath,"/figures/temp_conc_correlation/month_colored/",taxa,".png"),
         width=800,height=800,units="px",dpi=150)
}

#plot month in color
for(taxa in high_c_index$species){
  print(taxa)
  df_merge_wyear_mean %>% ggplot(aes(color=as.factor(month))) +
    geom_point(aes_string(x="Beam_temperature_corrected",y=taxa))+
    labs(y=expression("Carbon Concentration ("*mu*"g mL"^-1*")"),x=expression("Temperature ("*degree*"C)"))+
    scale_y_log10(limits=c(0.1,3007465))+ggtitle(taxa)+guides(color=guide_legend(title="Month"))+
    scale_color_manual(values=rainbow_mod)
  ggsave(filename=paste0(basepath,"/figures/temp_conc_correlation/high_cyclicity/month_colored/",taxa,".png"),
         width=800,height=800,units="px",dpi=150)
}



#plot month in color with GAM FIT 
for(taxa in label_maybe_include){
  print(taxa)
df_merge_wyear_mean %>% mutate(month=as.factor(month)) %>% 
  mutate_at(label_maybe_include,log10) %>%
  ggplot() +
  geom_point(aes_string(x="Beam_temperature_corrected",
                        y=taxa,color="month"))+
  scale_color_manual(values=rainbow_mod)+
  geom_smooth(aes_string(x="Beam_temperature_corrected",y=taxa),
              method="gam",color="black")+
  labs(y=expression("Log10-Transformed Carbon Concentration ("*mu*"g mL"^-1*")"),
       x=expression("Temperature ("*degree*"C)"))+ggtitle(taxa)+    ylim(0.01,6)+
  ylim(0.01,6)
ggsave(filename=paste0(basepath,"/figures/temp_conc_correlation/gam_fit/",taxa,".png"),
       width=800,height=800,units="px",dpi=150)
}

#plot month in color
for(taxa in high_c_index$species){
  print(taxa)
  df_merge_wyear_mean %>% mutate(month=as.factor(month)) %>% 
    mutate_at(label_maybe_include,log10) %>%
    ggplot() +
    geom_point(aes_string(x="Beam_temperature_corrected",
                          y=taxa,color="month"))+
    scale_color_manual(values=rainbow_mod)+
    geom_smooth(aes_string(x="Beam_temperature_corrected",y=taxa),
                method="gam",color="black")+
    ylim(0.01,6)+
    labs(y=expression("Log10-Transformed Carbon Concentration ("*mu*"g mL"^-1*")"),
         x=expression("Temperature ("*degree*"C)"))+ggtitle(taxa)
  ggsave(filename=paste0(basepath,"/figures/temp_conc_correlation/high_cyclicity/gam_fit/",taxa,".png"),
         width=800,height=800,units="px",dpi=150)
}

