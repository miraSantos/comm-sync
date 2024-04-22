
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","plotly","dtw","scales","patchwork")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
################
#GUINARDIA VIGNETTE = STANDARD DEVIATON
#############

df_cor_long <-df_cor %>% gather(species,cor,-year) %>% drop_na() %>% 
  filter(str_detect(species, "Guinardia"))


aov_res <- aov(cor ~ species, data=df_cor_long) 

aov_res%>% ggplot(aes(x = species, y = cor)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Group") + ylab("Scores")+
  geom_signif(comparisons = list(c("group_1", "group_2","group_3")), 
              map_signif_level=TRUE)


df_carbonC %>% mutate(week=as.factor(week))%>%
  mutate_at(protist_tricho_labelC,quadroot) %>%
  select(contains("Guinardia"))

g_striata <- df_carbonC_wyear_mean %>% ggplot() +
  geom_line(aes(x=week,y=Guinardia_striata,color = as.factor(year)))+
  xlab("Week of Year") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"^(1/4)))
  
g_flaccida <- df_carbonC_wyear_mean %>% ggplot() +
  geom_line(aes(x=week,y=Guinardia_flaccida,color = as.factor(year)))+
xlab("Week of Year") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"^(1/4)))
  

g_deli<- df_carbonC_wyear_mean %>% ggplot() +
  geom_line(aes(x=week,y=Guinardia_delicatula_merged,color = as.factor(year)))+
  xlab("Week of Year") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"^(1/4)))
  

cor_g_striata <-  ggplot(data=df_cor) + geom_point(aes_string(x="year",y="Guinardia_striata"))+
  geom_segment(aes_string(x="year",y=0,xend="year",yend="Guinardia_striata"))+
  geom_hline(aes(yintercept=0),color="black")+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-0.1,1)+
  ylab("Correlation Coefficient") +xlab("Year")

cor_g_flaccida <-  ggplot(data=df_cor) + geom_point(aes_string(x="year",y="Guinardia_flaccida"))+
  geom_segment(aes_string(x="year",y=0,xend="year",yend="Guinardia_flaccida"))+
  geom_hline(aes(yintercept=0),color="black")+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-0.1,1)+
  ylab("Correlation Coefficient") +xlab("Year")

cor_g_deli <-  ggplot(data=df_cor) + geom_point(aes_string(x="year",y="Guinardia_delicatula_merged"))+
  geom_segment(aes_string(x="year",y=0,xend="year",yend="Guinardia_delicatula_merged"))+
  geom_hline(aes(yintercept=0),color="black")+
  scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-0.1,1)+
  ylab("Correlation Coefficient") +xlab("Year")

combined_year <-  g_flaccida + g_deli + g_striata  & theme(legend.position = "right")
(combined_year +  plot_layout(guides = "collect",axis_titles="collect"))/(
   cor_g_flaccida + cor_g_deli + cor_g_striata + plot_layout(axis_titles="collect"))

ggsave(filename=paste0(basepath,"/figures/cyclic_index/guinardia_vignette.png"),
       width=2500,height=1500,units="px",dpi=300)
