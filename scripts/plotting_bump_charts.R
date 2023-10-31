base_path = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
load(paste0(basepath,"/data/ifcb/r_objects/filled/2023_Jul_28_dfcarbon_group.RData"))
load(paste0(basepath,"/data/ifcb/r_objects/filled/2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","tidyr","ggbump","purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

metseasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter")

seasons = metseasons[format(dfcarbon_class$date, "%m")]
dfcarbon_class$season = seasons
dfcarbon_class$season_numeric = NaN
dfcarbon_class$season_numeric[dfcarbon_class$season == "Winter"] = 1
dfcarbon_class$season_numeric[dfcarbon_class$season == "Spring"] = 2
dfcarbon_class$season_numeric[dfcarbon_class$season == "Summer"] = 3
dfcarbon_class$season_numeric[dfcarbon_class$season == "Fall"] = 4


seasons = metseasons[format(dfcarbon_group$date, "%m")]
dfcarbon_group$season = seasons

dfcarbon_group$season_numeric = NaN
dfcarbon_group$season_numeric[dfcarbon_group$season == "Winter"] = 1
dfcarbon_group$season_numeric[dfcarbon_group$season == "Spring"] = 2
dfcarbon_group$season_numeric[dfcarbon_group$season == "Summer"] = 3
dfcarbon_group$season_numeric[dfcarbon_group$season == "Fall"] = 4


##########################################
#RANK ORDER PLOTS
#generate rank order of top 10 diatoms within each regime
#retrieve IFCB classlist

dfcarbon_class$year = year(dfcarbon_class$date)
dfcarbon_class$month = factor(month(dfcarbon_class$date))

#creating regime column to sort rows into 3 regimes
dfcarbon_class$regime = NaN
regime_1_index = (which(dfcarbon_class$year < regime_1_end))
regime_2_index = (which((dfcarbon_class$year >= regime_1_end)&(dfcarbon_class$year < regime_2_end)))
regime_3_index = (which(dfcarbon_class$year >= regime_2_end))

dfcarbon_class$regime[regime_1_index] = paste0("2006 - ",as.character(regime_1_end-1))
dfcarbon_class$regime[regime_2_index] = paste0(as.character(regime_1_end)," - ",as.character(regime_2_end-1))
dfcarbon_class$regime[regime_3_index] = paste0(as.character(regime_2_end)," - 2022")


dfcarbon_class$doy_numeric <- yday(dfcarbon_class$date)
################ DIATOMS PER REGIME AND SEASON
groups = read.csv(url("https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/IFCB_classlist_type.csv"))
group = "Diatom"
names(groups)

plot_bump <- function(group) {
group_index=groups$CNN_classlist[(groups[group]==1)]
shared_index <- intersect(group_index,colnames(dfcarbon_class))
df_ranked <- dfcarbon_class %>% select(all_of(c("regime","season_numeric",shared_index))) %>%
  drop_na()%>%
  arrange(regime,season_numeric)%>%
  group_by(regime,season_numeric)%>% 
  summarize(across(all_of(shared_index),sum),.groups='keep')%>%
  pivot_longer(cols=shared_index,names_to=c("Species"),values_to=c("Concentration"))%>%
  group_by(regime,season_numeric) %>% 
  mutate(rank = rank(-Concentration,ties.method = "max"))%>%
  ungroup()

df_ranked$rank <- as.integer(df_ranked$rank)

n <- 14
ggplot(df_ranked[df_ranked$rank <= 5,], 
       aes(x = season_numeric, y = rank, color = Species,shape=Species)) +
  scale_shape_manual(values=1:n) +
  scale_color_manual(values = sample(rainbow(n)))+
  facet_grid(cols=vars(regime))+
  geom_bump(size=2)+
  geom_point(size=4)+
  scale_y_reverse()+
  xlab("Season")+
  ylab("Rank")

ggsave(filename=paste0(basepath,"/figures/bump-charts/bump-",group,"-regime.png"),
       width = 1000,height=500,units="px",dpi =130)
} 
plot_bump("Diatom")
plot_bump("Ciliate")
plot_bump("Dinoflagellate")
plot_bump("Nano")

plot_bump("Dinoflagellate")

### DIATOMS PER YEAR AND SEASON

df_ranked <- dfcarbon_class %>% select(all_of(c("year","season_numeric",shared_index))) %>%
  drop_na()%>%
  arrange(year,season_numeric)%>%
  group_by(year,season_numeric)%>% 
  summarize(across(all_of(shared_index),sum),.groups='keep')%>%
  pivot_longer(cols=shared_index,names_to=c("Species"),values_to=c("Concentration"))%>%
  group_by(year,season_numeric) %>% 
  mutate(rank = rank(-Concentration,ties.method = "max"))%>%
  ungroup()

df_ranked$rank <- as.integer(df_ranked$rank)

n <- 14
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

ggplot(df_ranked[df_ranked$rank <= 3,], aes(x = season_numeric, y = rank, color = Species,fill = Species)) +
  facet_grid(cols=vars(year))+
  geom_bump(size=2)+
  geom_point(size=3)+
  scale_y_reverse()+
  scale_color_manual(values = col_vector)

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\bump-diatoms-year.png",
       width = 4000,height=800,units="px",dpi =200)

x[x != "b"]  # without elements that are "b"




### DINO PER YEAR AND SEASON
shared_index <- intersect(dino_index,colnames(dfcarbon_class))
shared_index <- shared_index[shared_index != "Dinophyceae"]
df_ranked <- dfcarbon_class %>% select(all_of(c("year","season_numeric",shared_index))) %>%
  drop_na()%>%
  arrange(year,season_numeric)%>%
  group_by(year,season_numeric)%>% 
  summarize(across(all_of(shared_index),sum),.groups='keep')%>%
  pivot_longer(cols=shared_index,names_to=c("Species"),values_to=c("Concentration"))%>%
  group_by(year,season_numeric) %>% 
  mutate(rank = rank(-Concentration,ties.method = "max"))%>%
  ungroup()

df_ranked$rank <- as.integer(df_ranked$rank)

n <- 14
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

ggplot(df_ranked[df_ranked$rank <= 3,], aes(x = season_numeric, y = rank, color = Species,fill = Species)) +
  facet_grid(cols=vars(year))+
  geom_bump(size=2)+
  geom_point(size=3)+
  scale_y_reverse()+
  scale_color_manual(values = col_vector)

ggsave(filename="C:\\Users\\Miraflor P Santos\\comm-sync\\figures\\bump-dino-year.png",
       width = 4000,height=800,units="px",dpi =170)



#FUNCTIONAL GROUPS
names(dfcarbon_group)

shared_index <- c("Diatom_noDetritus","Dinoflagellate","Ciliate","NanoFlagCocco")
df_ranked <- dfcarbon_group %>% select(all_of(c("regime","season_numeric",shared_index))) %>%
  drop_na()%>%
  arrange(regime,season_numeric)%>%
  group_by(regime,season_numeric)%>% 
  summarize(across(all_of(shared_index),sum),.groups='keep')%>%
  pivot_longer(cols=shared_index,names_to=c("Species"),values_to=c("Concentration"))%>%
  group_by(regime,season_numeric) %>% 
  mutate(rank = rank(-Concentration,ties.method = "max"))%>%
  ungroup()

df_ranked$rank <- as.integer(df_ranked$rank)

n <- 4
ggplot(df_ranked, 
       aes(x = season_numeric, y = rank, color = Species,shape=Species)) +
  scale_shape_manual(values=1:n) +
  scale_color_manual(values = sample(rainbow(n)))+
  facet_grid(cols=vars(regime))+
  geom_bump(size=2)+
  geom_point(size=4)+
  scale_y_reverse()+
  xlab("Season")+
  ylab("Rank")


ggsave(filename=paste0(basepath,"/figures/bump-charts/bump-","protist","-regime.png"),
       width = 1000,height=500,units="px",dpi =130)

