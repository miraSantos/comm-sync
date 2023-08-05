save_r_path = "C:\\Users\\Miraflor P Santos\\comm-sync\\data\\ifcb\\r_objects\\unfilled\\"
load(paste0(save_r_path,"2023_Jul_28_dfcarbon_group.RData"))
load(paste0(save_r_path,"2023_Jul_28_dfcount_index.RData"))

list.of.packages <- c("RColorBrewer", "lubridate","ggplot2",
                      "tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","ggbump","purrr")
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


shared_diatoms <- intersect(diatom_index,colnames(dfcarbon_class))
df_ranked <- dfcarbon_class %>% select(all_of(c("regime","month",shared_diatoms))) %>%
  group_by(regime,month) %>% 
  summarize(across(all_of(shared_diatoms),sum))%>%
  pivot_longer(cols=shared_diatoms,names_to=c("Species"),values_to=c("Concentration")) %>%
  group_by(regime,month)%>%
  mutate(rank = rank(-Concentration))%>%
  ungroup()

df_ranked$month <- as.numeric(df_ranked$month)
df_ranked$rank <- as.integer(df_ranked$rank)

ggplot(df_ranked, aes(x = month, y = rank, color = Species)) +
  facet_grid(cols=vars(regime))+
  ylim(3,1)+
  geom_bump(size=2)+
  scale_x_continuous(breaks=seq(1,12,1))+
  scale_fill_brewer(palette="Set3")


shared_diatoms <- intersect(diatom_index,colnames(dfcarbon_class))
df_ranked <- dfcarbon_class %>% select(all_of(c("regime","season_numeric",shared_diatoms))) %>%
  drop_na()%>%
  group_by(regime,season_numeric) %>% 
  summarize(across(all_of(shared_diatoms),sum))%>%
  pivot_longer(cols=shared_diatoms,names_to=c("Species"),values_to=c("Concentration"))%>%
  group_by(regime,season_numeric)%>%
  mutate(rank = rank(-Concentration))%>%
  ungroup()

df_ranked$rank <- as.integer(df_ranked$rank)

ggplot(df_ranked, aes(x = season_numeric, y = rank, color = Species)) +
  facet_grid(cols=vars(regime))+
  geom_bump(size=2)+
  scale_fill_brewer(palette="Set3")


groups <- c("Diatom_noDetritus","Dinoflagellate","NanoFlagCocco","Ciliate","metazoan")
df_ranked <- dfcarbon_group %>% select(all_of(c("regime","season_numeric",groups))) %>%
  drop_na()%>%
  group_by(regime,season_numeric) %>% 
  summarize(across(all_of(groups),sum))%>%
  pivot_longer(cols=groups,names_to=c("func_group"),values_to=c("Concentration"))%>%
  group_by(regime,season_numeric)%>%
  mutate(rank = rank(-Concentration))%>%
  ungroup()

df_ranked$rank <- as.integer(df_ranked$rank)

ggplot(df_ranked, aes(x = season_numeric, y = rank, color = func_group)) +
  facet_grid(cols=vars(regime))+
  geom_bump(size=2)+
  scale_y_reverse(limits = c(5,1))+
  scale_fill_brewer(palette="Set3")


groups <- c("Diatom_noDetritus","Dinoflagellate","NanoFlagCocco","Ciliate","metazoan")
df_ranked <- dfcarbon_group %>% select(all_of(c("year","season_numeric",groups))) %>%
  drop_na()%>%
  group_by(regime,season_numeric) %>% 
  summarize(across(all_of(groups),sum))%>%
  pivot_longer(cols=groups,names_to=c("func_group"),values_to=c("Concentration"))%>%
  group_by(regime,season_numeric)%>%
  mutate(rank = rank(-Concentration))%>%
  ungroup()

df_ranked$rank <- as.integer(df_ranked$rank)

ggplot(df_ranked, aes(x = season_numeric, y = rank, color = func_group)) +
  facet_grid(cols=vars(regime))+
  geom_bump(size=2)+
  scale_y_reverse(limits = c(5,1))+
  scale_fill_brewer(palette="Set3")



groups <- c("Diatom_noDetritus","Dinoflagellate","NanoFlagCocco","Ciliate","metazoan")
df_ranked <- dfcarbon_group %>% select(all_of(c("year","season_numeric",groups))) %>%
  drop_na()%>%
  group_by(year,season_numeric) %>% 
  summarize(across(all_of(groups),sum))%>%
  pivot_longer(cols=groups,names_to=c("func_group"),values_to=c("Concentration"))%>%
  group_by(year,season_numeric)%>%
  mutate(rank = rank(-Concentration))%>%
  ungroup()

df_ranked$rank <- as.integer(df_ranked$rank)

ggplot(df_ranked, aes(x = season_numeric, y = rank, color = func_group)) +
  facet_grid(cols=vars(year))+
  geom_bump(size=2)+
  ylim(1,5)+
  scale_y_reverse()+
  scale_fill_brewer(palette="Set3")
