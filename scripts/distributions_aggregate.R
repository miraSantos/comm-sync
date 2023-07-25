# install.packages(c("RColorBrewer",
#                    "lubridate","fields","ggplot2","tibbletime","dplyr","sets",
#                    "reshape2","ggformula","wsyn","tidyr","moments"))


library(latex2exp)
library("RColorBrewer")
library("lubridate")
library("fields")
library("ggplot2")
library("tibbletime")
library("dplyr")
library("sets")
library("reshape2")
library("ggformula")
library("wsyn")
library("tidyr")
library("moments")


####################################

url = "https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/biovol_concentration_by_class_time_series_CNN_daily10Jan2022.csv"
df<-read.csv(url(url))

df$date=as.Date(df$date,format="%d-%b-%Y %H:%M:%S")
df$doy_numeric=yday(df$date)
nan_list= sapply(df, function(x) sum(is.na(x)))
hist(nan_list)

groups = read.csv(url("https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/IFCB_classlist_type.csv"))
head(groups)
df[sapply(df, is.nan)] <- NA
head(df)

env_url = "https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/mvco_env_table.csv"

df_env <- read.csv(url(env_url))
df_env$date <- as.Date(df_env$time_local,format="%d-%b-%Y %H:%M:%S")
df_env$doy_numeric=yday(df_env$date)
head(df_env)

df_env=df_env %>% group_by(date) %>%   summarise(across(c(wind_speed,wind_dir,solar,temp_beam,salinity_beam),function(x) mean(x, na.rm=TRUE)),.groups="drop")
head(df_env)

df <- merge(df,df_env,by="date")
env_index= colnames(df_env)[colnames(df_env) != "date"]

###################################
create_groups <- function(func_group){
  reference=groups$CNN_classlist[(groups[func_group]==1)]
  index=intersect(colnames(df),reference)
  return(index)
}

diatom_index <- create_groups("Diatom")
dino_index <- create_groups("Dinoflagellate") #TO DO: exclude dinophycaeae
cocco_index <- create_groups("Coccolithophore")
cilia_index <- create_groups("Ciliate")
flagellate_index <- create_groups("flagellate")

all_index=c(diatom_index,dino_index,cocco_index,cilia_index,flagellate_index)

cilia_index = cilia_index[cilia_index != "ciliate"] 


#####################################

df_freq <- df %>% 
  #set to daily frequency
  complete(date = seq.Date(min(df$date),max(df$date), by="day")) %>%
  #fill out doy_numeric
  mutate(doy_numeric = yday(date)) %>%
  group_by(doy_numeric)%>%
  #replace nans for living things with yearly mean
  mutate(across(all_of(c(all_index,env_index)),~replace_na(.,mean(.,na.rm=T))))
nan_list= sapply(df_freq, function(x) sum(is.na(x)))
hist(nan_list)


##########################

df_freq["diatom"] = rowSums(df_freq[diatom_index])
df_freq["cocco"] = rowSums(df_freq[cocco_index])
df_freq["dino"] = rowSums(df_freq[dino_index])
df_freq["cilia"] = rowSums(df_freq[cilia_index])
df_freq["flagellate"] = rowSums(df_freq[flagellate_index])

#creating index markers for all individual live taxa and for all the functional groups
df_freq$all = rowSums(df_freq[all_index],na.rm=TRUE)
all_sum_index = c("diatom","cocco","dino","cilia","flagellate")
df_freq$all_func = rowSums(df_freq[all_sum_index],na.rm=T)

#####################################


long = melt(df_freq[c("all_func","cocco","dino","cilia","diatom","flagellate","date")],
            id.vars = "date",
            value.name="biovol",
            variable.name="type")

long$biovol = log10(long$biovol)

sp <- split(long$biovol, long$type)
a <- lapply(seq_along(sp), function(i){
  d <- density(sp[[i]])
  k <- which.max(d$y)
  data.frame(cyl = names(sp)[i], xmax = d$x[k], ymax = d$y[k])
})
a <- do.call(rbind, a)

#########################################
##PLOT###################################

options(repr.plot.width = 8, repr.plot.height =5)
ggplot(long, aes(biovol)) + 
  geom_density(aes(fill = factor(type)),lwd=0.1, alpha = 0.5) +
  geom_text(data = a, 
            aes(x = xmax, y = ymax, 
                label = c("All Functional Groups","Coccolithophore","Dinoflagellate","Ciliates","Diatoms","Flagellates"), vjust = -0.5))+
  annotation_logticks(sides="b")+
  scale_x_continuous(breaks = seq(0,6,1),
                     labels  =scales::math_format(10^.x))+ 
  theme(axis.text=element_text(size = 15),
        axis.title=element_text(size = 20),
        plot.title = element_text(size=25),
        legend.position="none")+
  xlab(TeX("Biovolume $(\\mu m^{3} mL^{-1})$"))+
  ylab("Density")+
  ggtitle("Distribution of Aggregate Biovolumes")
