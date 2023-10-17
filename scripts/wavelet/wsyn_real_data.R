list.of.packages <- c("RColorBrewer","lubridate","fields","ggplot2",
                      "tibbletime","dplyr","sets","reshape2","ggformula",
                      "wsyn","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)



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

dates = seq(min(df$date),max(df$date), by="1 day")
num_missing=length(dates)-length(df$date)
percent_missing = num_missing/length(dates)
print(num_missing)
print(c("percent missing:",percent_missing))

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


df_freq["diatom"] = rowSums(df_freq[diatom_index])
df_freq["cocco"] = rowSums(df_freq[cocco_index])
df_freq["dino"] = rowSums(df_freq[dino_index])
df_freq["cilia"] = rowSums(df_freq[cilia_index])
df_freq["flagellate"] = rowSums(df_freq[flagellate_index])

#creating index markers for all individual live taxa and for all the functional groups
df_freq$all = rowSums(df_freq[all_index],na.rm=TRUE)
all_sum_index = c("diatom","cocco","dino","cilia","flagellate")
df_freq$all_func = rowSums(df_freq[all_sum_index],na.rm=T)

index = all_sum_i
group_title="Functional Groups"

plotmags <- function(group_title,index){
dat=t(data.matrix(df_freq[index]))
times = seq(1,ncol(dat),1)
title = paste("Synchrony in ",group_title)

dat<-cleandat(dat,times,4)$cdat

cmat<-cor(t(dat))
diag(cmat)<-NA
cmat<-as.vector(cmat)
cmat<-cmat[!is.na(cmat)]
hist(cmat,30,xlab="Pearson correlation",ylab="Count")

res<-wpmf(dat,times,sigmethod="quick")

jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
               "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
colorfill<-grDevices::colorRampPalette(jetcolors)
pal = colorRampPalette(brewer.pal(9, 'Oranges'))(1024)

zwav = Mod(res$values)
sigthresh = 0.95
q = stats::quantile(res$signif[[2]],sigthresh)

inds<-which(!is.na(colMeans(zwav,na.rm=T)))
zwav<-zwav[,inds]
timescales<-res$timescales[inds]
ylocs <- pretty(timescales, n = 8)
options(repr.plot.width = 15, repr.plot.height =5)

dates = seq(as.Date("2006/1/1"),as.Date("2021/1/1"),by="years")

image.plot(x=df_freq$date,y=timescales,z=zwav,
           xlab="Time",ylab="Timescale (Days)",main=paste(title),
           ylim=c(7,2000),zlim=c(0,1),
           col=pal,
           log="y",axes=F)
box()
axis(1,at = dates,label = format(dates,"%Y"))
axis(2,at=c(1:2 %o% 10^(1:3)),label =c(1:2 %o% 10^(1:3)) )
axis(2,at=c(365),label=c(365),col.axis="red")
contour(x=df_freq$date,y=timescales,z=zwav,add=T,color="blue",log="y",axes=F,levels=q,drawlabels=F,lwd=2)

options(repr.plot.width = 8, repr.plot.height =5)
h<-power(res)
coh <- data.frame(x = -log(1/h$timescales),y = h$power)
xlocs<-c(1:3 %o% 10^(1:3))
ggplot(data = coh,aes(x = x, y = y)) + 
  geom_line()+
  annotation_logticks(sides="b")+
  scale_x_continuous(breaks = c(-log(1/xlocs),365),labels=c(xlocs,365))+
  xlab("Timescales")+
  ylab("Power")+
  ggtitle(paste("Coherence in",group_title))

pal = colorRampPalette(brewer.pal(11, 'Oranges'))(1024)

sm<-synmat(dat,times,method="ReXWT")
options(repr.plot.width = 6, repr.plot.height =6)
par(mar=c(7,7,2,1)+.1) #mar is margin (bottom,left,top,right)
fields::image.plot(1:length(index),1:length(index),sm,col=pal,axes = F,ylab="",xlab="",main="Synchrony Matrix")
axis(2,at=seq(1,length(index),1),label=index,las = 1)
axis(1,at=seq(1,length(index),1),label=index,las = 3)
}

plotmags("Cluster 4",group4)


#### CLUSTERING
index = all_index
dat=t(data.matrix(df_freq[index]))
times = seq(1,ncol(dat),1)


