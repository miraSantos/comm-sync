#Loading Packages
list.of.packages <- c("RColorBrewer","lubridate","fields","ggplot2",
                      "tibbletime","dplyr","sets","reshape2","ggformula",
                      "wsyn","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

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


means = colMeans(df_freq[diatom_index][sapply(df_freq[diatom_index], is.numeric)])
top10diatom_index=names(sort(means,decreasing=TRUE))[1:10]
top20diatom_index=names(sort(means,decreasing=TRUE))[1:20]
top10diatom_index_sansg = names(sort(means,decreasing=TRUE))[2:10]

means = colMeans(df_freq[dino_index][sapply(df_freq[dino_index], is.numeric)], na.rm=TRUE)
top10dino_index=names(sort(means,decreasing=TRUE))[1:10]

means = colMeans(df_freq[cilia_index][sapply(df_freq[cilia_index], is.numeric)], na.rm=TRUE)
top10cilia_index=names(sort(means,decreasing=TRUE))[1:10]

means = colMeans(df_freq[flagellate_index][sapply(df_freq[flagellate_index], is.numeric)], na.rm=TRUE)
top10flag_index=names(sort(means,decreasing=TRUE))[1:10]


# x = c()
# y = c()
# for( column in diatom_index){
dat = df_freq$wind_speed
title="Wavelet Transform of Wind Speed at MVCO from 2006-2021"
times = seq(1,length(dat),1)
dat<-cleandat(dat,times,clev=5)$cdat
res <- wt(dat,times)
zwav = Mod(res$values)
# append(x,colMeans(Re(res$values),na.rm=T))
# append(y,colMeans(Im(res$values),na.rm=T))
# print(column)
# }
jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
               "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
colorfill<-grDevices::colorRampPalette(jetcolors)

sigthresh = c(0.95,0.99)
q = stats::quantile(res$signif[[2]],sigthresh)

inds<-which(!is.na(colMeans(zwav,na.rm=T)))
zwav<-zwav[,inds]
timescales<-res$timescales[inds]
ylocs <- pretty(timescales, n = 8)

image.plot(x=df_freq$date,y=timescales,z=zwav,
           xlab="Time (Years)",ylab="Timescale (Days)",main=paste(title),
           col=colorfill(100),
           log="y",axes=F)
box()
dates = seq(as.Date("2006/1/1"),as.Date("2021/1/1"),by="years")

axis(1,at = dates,label = format(dates,"%Y"))
axis(2,at=c(1:2 %o% 10^(1:3)),label =c(1:2 %o% 10^(1:3)) )
axis(2,at=c(365),label=c(365),col.axis="red")
contour(x=df_freq$date,y=timescales,z=zwav,
        labels=c("95%","99%"),
        method="simple",
        add=T,levels=q,drawlabels = F,
        lty=c("solid","dashed"))



############################################
# temp = matrix(df_freq$temp_beam, nrow=length(index), ncol=length(df_freq$temp_beam), byrow=TRUE)
# dat<-cleandat(temp,times,clev=1)$cdat
# 
# res_temp<- wpmf(dat,times,sigmethod="quick")
# plotmag(res_temp)

