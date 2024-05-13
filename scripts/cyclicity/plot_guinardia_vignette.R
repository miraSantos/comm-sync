
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork","car","graphics","stats")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"/data/r_objects/c_index_df_cor_2024)22_april.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/unfilled/2024_Apr_19_df_carbonC.RData"))


###############################################
df_carbonC$year <- year(df_carbonC$date)
df_carbonC$week <- week(df_carbonC$date)
df_carbonC$wyear <- paste0(df_carbonC$week,"-",df_carbonC$year)

df_carbonC_wyear_mean <-df_carbonC %>% group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup()

df_carbonC_wyear_mean$year <- factor(df_carbonC_wyear_mean$year)

quadroot <- function(x){x^(1/4)}


################
#GUINARDIA VIGNETTE analysis of variance
#############

df_cor_long <-df_cor %>% gather(species,cor,-year) %>% drop_na() %>% 
  filter(str_detect(species, "Guinardia"))

df_carbon_long <- df_carbonC_wyear_mean %>% gather(species,conc,-c(year,wyear,date,week,doy_numeric,pid)) %>% drop_na() %>% 
  filter(str_detect(species, "Guinardia"))

#test for homogeneity of variance
ftest_1 <- var.test(df_cor$Guinardia_striata,df_cor$Guinardia_flaccida,alternative = "less")
ftest_2 <- var.test(df_cor$Guinardia_striata,df_cor$Guinardia_delicatula_merged,alternative="less")


df_cor_long %>% ggplot(aes(x = species, y = cor)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete(labels=c("Guinardia delicatula","Guinardia flaccida","Guinardia striata")) +
  xlab("Species") + ylab("Correlation Coefficient")+
  geom_signif(annotation = paste0(expression("Var(gs) < Var(gf), p="),formatC(ftest_1$p.value,digits=1)),y_position = 1, xmin = 2, xmax = 3,
              tip_length = c(0.08, 0.04))+
  geom_signif(annotation = paste0("Var(gs) < Var(gd), p=",formatC(ftest_2$p.value,digits=1)),y_position = 1.2, xmin = 1, xmax = 3,
              tip_length = c(0.2, 0.04))+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/guinardia_vignette_sig_variance.png"),
       width=1500,height=1200,units="px",dpi=300)

###########################################################################
g_striata <- df_carbonC_wyear_mean %>% ggplot() +
  geom_line(aes(x=week,y=Guinardia_striata,color = as.factor(year)))+
  xlab("Week of Year") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))
  
g_flaccida <- df_carbonC_wyear_mean %>% ggplot() +
  geom_line(aes(x=week,y=Guinardia_flaccida,color = as.factor(year)))+
xlab("Week of Year") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))
  

g_deli<- df_carbonC_wyear_mean %>% ggplot() +
  geom_line(aes(x=week,y=Guinardia_delicatula_merged,color = as.factor(year)))+
  xlab("Week of Year") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))
  

cor_g_striata <-  ggplot(data=df_cor) +
  geom_point(aes_string(x="year",y="Guinardia_striata"))+
  geom_line(aes_string(x="year",y="Guinardia_striata"))+
    geom_hline(aes(yintercept=0),color="red")+
  scale_x_continuous(breaks=seq(2006,2023,4))+ylim(-0.1,1)+
  ylab("Correlation Coefficient") +xlab("Year")

cor_g_flaccida <-  ggplot(data=df_cor) +
  geom_point(aes_string(x="year",y="Guinardia_flaccida"))+
  geom_line(aes_string(x="year",y="Guinardia_flaccida"))+
  geom_hline(aes(yintercept=0),color="red")+
  scale_x_continuous(breaks=seq(2006,2023,4))+ylim(-0.1,1)+
  ylab("Correlation Coefficient") +xlab("Year")

cor_g_deli <-  ggplot(data=df_cor) + 
  geom_point(aes_string(x="year",y="Guinardia_delicatula_merged"))+
  geom_line(aes_string(x="year",y="Guinardia_delicatula_merged"))+
  geom_hline(aes(yintercept=0),color="red")+
  scale_x_continuous(breaks=seq(2006,2023,4))+ylim(-0.1,1)+
  ylab("Correlation Coefficient") +xlab("Year")

combined_year <-  g_flaccida + g_deli + g_striata  &
  theme(legend.position = "right") &
  scale_x_continuous(breaks=seq(0,53,4)) &
  guides(color=guide_legend(ncol=2)) &labs(color="Year")
(combined_year +  plot_layout(guides = "collect",axis_titles="collect"))/(
   cor_g_flaccida + cor_g_deli + cor_g_striata + 
     plot_layout(axis_titles="collect"))+
  plot_annotation(tag_levels = 'A')&  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.75, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

ggsave(filename=paste0(basepath,"/figures/cyclic_index/guinardia_vignette.png"),
       width=2500,height=1500,units="px",dpi=250)


var.test(df_cor$Guinardia_delicatula_merged,df_cor$Guinardia_striata)

