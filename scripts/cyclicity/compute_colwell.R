basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("tidyr","tibble","hydrostats")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"/data/r_objects/unfilled/2024-09-11_df_carbonC.RData"))
source(paste0(basepath,"scripts/cyclicity/colwells.R"))
metseasons <- c(
  "01" = "DJF", "02" = "DJF",
  "03" = "MAM", "04" = "MAM", "05" = "MAM",
  "06" = "JJA", "07" = "JJA", "08" = "JJA",
  "09" = "SON", "10" = "SON", "11" = "SON",
  "12" = "DJF")

#add seasons and weeks etc to time series
seasons = metseasons[format(df_carbonC$date, "%m")]
df_carbonC <- df_carbonC %>% mutate(doy_numeric = yday(date),
                                    week = week(date),year=year(date),
                                    wyear=paste0(year,"-",week),
                                    season=seasons,
                                    syear=paste0(year,"-",season))



#create version of data at weekly time scale
df.week <-df_carbonC %>%
  group_by(wyear) %>%
  mutate_at(protist_tricho_labelC,mean,na.rm=T) %>%
  distinct(wyear, .keep_all=TRUE) %>%
  ungroup() %>%
  mutate_at(protist_tricho_labelC,log_zero)

df.week %>% ggplot() + geom_point(aes(x=week,y=Synechococcus))
df.week %>% ggplot() + geom_point(aes(x=week,y=Gonyaulax))
df.week %>% ggplot() + geom_point(aes(x=week,y=Cylindrotheca_merged))


func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton")

super_colwell <- list()
super_P <- list()
super_C <- list()
super_M <- list()
super_GC <- list()
super_GP <- list()
super_GM <- list()

for(taxa in protist_tricho_labelC){
#create data ts object with date and abundance column
data.ts <- data.frame(df.week[c("date",taxa)])

#convert date object to date 
data.ts$date<-as.POSIXlt(data.ts$date,format="%Y-%m-%d")

#rename column names
colnames(data.ts) <- c("Date","Q")

#compute colwell statistics
result <-Colwells(data.ts,fn="mean",boundaries="equal",s=9)

#store in dataframes
super_colwell[[taxa]] <- result
super_P[taxa] <- result$P
super_C[taxa] <- result$C
super_M[taxa] <- result$M
super_GC[taxa] <- result$GC
super_GP[taxa] <- result$GP
super_GM[taxa] <- result$GM
}

head(super_P)
df.colwell <- data.frame(taxa = protist_tricho_labelC,
                   p = unlist(super_P),
                   c = unlist(super_C),
                   m = unlist(super_M),
                   gm = unlist(super_GM),
                   gc = unlist(super_GC),
                   gp = unlist(super_GP))

func_group_list = c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton",
                    "Metazoan","Synechococcus","Picoeukaryotes")
func_group_labels <- list(diatom_labelC,dino_label,ciliate_label,nfg_label,
                          metazoan_label,c("Synechococcus"),c("Pico_eukaryotes"))
#create column with functional group 
for(func_group in 1:length(func_group_list)){
  reference=func_group_labels[[func_group]]
  df.colwell[df.colwell$taxa%in%reference,"func_group"] = func_group_list[func_group]
}

my_colors <- RColorBrewer::brewer.pal(7, "Dark2")
map <- data.frame(func_group=func_group_list,colors=my_colors)
map_dict <- map$colors
names(map_dict) <- map$func_group


#look at flow table
print(super_colwell[["Synechococcus"]]$flow.table)

df.week %>% ggplot() + 
  geom_point(aes(x = week, y = Ditylum_brightwellii,
                                    color=as.factor(year)))
head(df.colwell)

chi2 = list()
t = 53
s = 9
chi2["gp"] = qchisq(0.95,df=t*(s-1),lower.tail=F)
chi2["gm"] = qchisq(0.95,df=(s-1)*(t-1),lower.tail=F)
chi2["gc"] = qchisq(0.95,df=(s-1),lower.tail=F)
chi2
super_colwell[["Ditylum_brightwellii"]]


df.colwell$gp > chi2$gp

df.colwell %>% filter(taxa %in% label_maybe_include,gp > chi2$gp) %>%
  ggplot() +
  geom_bar(aes(x=reorder( taxa,-p),y = p,fill=func_group),stat="identity") + 
   labs(x = "Taxa",y="Predictability (P)")+
                        scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
    plot.margin=unit(c(0,0,0,1.5),units="cm"))

#barplot of constancy                
df.colwell %>% filter(taxa %in% label_maybe_include,gc>chi2$gc)%>% 
  ggplot() +
  geom_bar(aes(x=reorder( taxa,+c),y = c,fill=func_group),stat="identity") + 
  coord_flip() + labs(x ="Taxa", y = "Constancy (C)") +
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  ylim(0,1) + 
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
  )
ggsave(filename=paste0(basepath,"figures/colwell/constancy_",Sys.Date(),".png"),
       width = 2500,height = 2000,
       units="px",dpi = 300)

#barplot of contingency (horizontal)
df.colwell %>% filter(taxa %in% label_maybe_include,gm >chi2$gm)%>% 
  ggplot() +
  geom_bar(aes(x=reorder( taxa,-m),y = m,fill=func_group),stat="identity") + 
   labs(x ="Taxa", y = "Contingency (M)") + 
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95),
    plot.margin=unit(c(0,0,0,1.5),units="cm"))

ggsave(filename=paste0(basepath,"/figures/colwell/contingency_horizontal_",Sys.Date(),".png"),
       width = 4300,height = 2500, units = "px",
       dpi = 300)

#barplot of contingency
df.colwell %>% filter(taxa %in% label_maybe_include,gm >chi2$gm)%>% 
  ggplot() +
  geom_bar(aes(x=reorder( taxa,-m),y = m,fill=func_group),stat="identity") + 
  coord_flip() + 
   labs(x ="Taxa", y = "Contingency (M)") + 
  scale_fill_manual(values=map_dict,name="Functional\nGroup")+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major  = element_line(size = 0.25, linetype = 'solid',
                                     colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
  )
ggsave(filename=paste0(basepath,"/figures/colwell/contingency_",Sys.Date(),".png"),
       width = 2500,height = 4300, units = "px",
       dpi = 300)

dfcg<-df.colwell %>%
  mutate(taxa = case_when(taxa == "Diatom_noDetritus"~"Diatom",
                          taxa == "NanoFlagCocco"~"Misc. Nanoplankton",
                          .default = taxa)) %>%
  filter(taxa %in% c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton")) %>% 
  group_by(taxa) %>% summarize(m=m,p) %>% 
  rename(func_group= taxa)



df.colwell %>%
  filter(func_group %in%  c("Diatom","Dinoflagellate","Ciliate","Misc. Nanoplankton")) %>%
  group_by(taxa) %>% 
  ggplot() +  geom_histogram(aes(x=m,
                                 y=after_stat(count/ave(count, PANEL, FUN = sum))),
                             color="white",fill="#009E73",bins=12)+
  facet_grid(rows=vars(as.factor(func_group)))+
  labs(x = "Contingency (M)",y="Percent")+
  scale_y_continuous(labels = scales::percent)+
  theme(
    panel.background = element_rect(fill = "white", colour = "black",
                                    linewidth = 0.75, linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "gray"), 
    panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +     geom_vline(data =dfcg,
                     aes(xintercept=mean),color="#E69F00",linewidth=1.5)

ggsave(filename=paste0(basepath,"figures/colwell_contingency_m_",Sys.Date(),".png"),
       width=1000,height=2300,units="px",dpi=300)


df.week %>% ggplot() + 
  geom_point(aes(x = week, y = nanoplankton_mix,
                 color=as.factor(year)))

