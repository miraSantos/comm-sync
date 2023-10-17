list.of.packages <- c("RColorBrewer","lubridate","fields",
                   "ggplot2","tibbletime","dplyr","sets",
                   "reshape2","ggformula","wsyn","tidyr","moments")

#install packages if they aren't already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(list.of.packages, require, character.only = TRUE)
####################################
data_url = "https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/biovol_concentration_by_class_time_series_CNN_daily10Jan2022.csv"
df<-read.csv(url(data_url))

#formating date and creating column for day of year
df$date=as.Date(df$date,format="%d-%b-%Y %H:%M:%S")
df$doy_numeric=yday(df$date)

#loading in classlist that groups taxa into functional groups
groups = read.csv(url("https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/IFCB_classlist_type.csv"))
head(groups)


df[sapply(df, is.nan)] <- NA
head(df)

env_url = "https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/mvco_env_table.csv"

df_env <- read.csv(env_url,sep = ",",header=T)
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

a= cbind(diatom_index,rep("diatom",length(diatom_index)))
b= cbind(dino_index,rep("dino",length(dino_index)))
c= cbind(cocco_index,rep("coccolithophore",length(cocco_index)))
d= cbind(cilia_index,rep("cliate",length(cilia_index)))
e= cbind(flagellate_index,rep("flagellate",length(flagellate_index)))

all_index_flagged = rbind(a,b,c,d,e)
cilia_index = cilia_index[cilia_index != "ciliate"] 

func_group="Diatom"
reference=groups$CNN_classlist[(groups[func_group]==1)]
index=intersect(colnames(df),reference)
#####################################

df_freq <- df %>% 
  #set to daily frequency
  complete(date = seq.Date(min(df$date),max(df$date), by="day")) %>%
  #fill out doy_numeric
  mutate(doy_numeric = yday(date)) %>%
  group_by(doy_numeric)%>%
  #replace nans for living things with yearly mean
  mutate(across(all_of(c(all_index)),~replace_na(.,mean(.,na.rm=T))))
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

#####################################
####

bloom_group = c("Thalassiosira_sp_aff_mala", "Tintinnida","Warnowia","Tontonia_appendiculariformis" ,"Syracosphaera_pulchra","Strombidium_morphotype1","Scrippsiella","Rhabdosphaera","Protoperidinium","Polykrikos","Ophiaster" ,"Nanoneis","Lauderia_annulata","Katodinium_or_Torodinium","Karenia","Hemiaulus","Euplotes","Alexandrium_catenella","Akashiwo")
diatom_special= c("Ephemera","pennate","Bacillaria","Biddulphia","Thalassionema","Rhizosolenia","Pleurosigma","Paralia_sulcata","Guinardia_striata","Guinardia_delicatula","Eucampia","Delphineis","Coscinodiscus")
index =bloom_group
group_title="Wavelet Cluster 3"

dat=t(data.matrix(df_freq[index]))
times = seq(1,ncol(dat),1)
title = paste("Wavelet Phasor Mean Field of ",group_title)

dat<-cleandat(dat,times,clev=5)$cdat

res<-wpmf(dat,times,sigmethod="quick")

jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
               "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
colorfill<-grDevices::colorRampPalette(jetcolors)

zwav = Mod(res$values)

sigthresh = c(0.01,0.05,0.95,0.99)
q = stats::quantile(res$signif[[2]],sigthresh)

inds<-which(!is.na(colMeans(zwav,na.rm=T)))
zwav<-zwav[,inds]
timescales<-res$timescales[inds]
ylocs <- pretty(timescales, n = 8)
options(repr.plot.width = 15, repr.plot.height =5)

dates = seq(as.Date("2006/1/1"),as.Date("2021/1/1"),by="years")
plot.new()
par(mar=c(5,4,4,1))
image.plot(x=df_freq$date,y=timescales,z=zwav,
           xlab="Time (Years)",ylab="Timescale (Days)",main=paste(title),
           xlim=c(as.Date("2006-01-01"),as.Date("2022-01-01")),
           ylim=c(30,2000),
           zlim=c(0,1),
           col=colorfill(100),
           log="y",axes=F)
box()
axis(1,at = dates,label = format(dates,"%Y"))
axis(2,at=c(1:2 %o% 10^(1:3)),label =c(1:2 %o% 10^(1:3)) )
axis(2,at=c(365),label=c(365),col.axis="red")
contour(x=df_freq$date,y=timescales,z=zwav,
        labels=c("1%","5%","95%","99%"),
        method="simple",
        add=T,levels=q,drawlabels = F,
        lty=c("longdash","solid","solid","dashed"))
######################################

bottom = 0.4
left = 1
top = 0.2
right = 0.4
par(mfrow=c(2,1),mai=c(bottom,left, top, right)) #bottom, left, top, right
image.plot(x=df_freq$date,y=timescales,z=zwav,
           xlab="Time (Years)",ylab="Timescale (Days)",main=paste(title),
           xlim=c(as.Date("2006-01-01"),as.Date("2022-01-01")),
           ylim=c(30,2000),zlim=c(0,1),
           col=colorfill(100),
           log="y",axes=F,
           smallplot = c(.93, .94, .2, .8)) #left, right, top, bottom
box()
axis(1,at = dates,label = format(dates,"%Y"))
axis(2,at=c(1:2 %o% 10^(1:3)),label =c(1:2 %o% 10^(1:3)) )
axis(2,at=c(365),label=c(365),col.axis="red")
contour(x=df_freq$date,y=timescales,z=zwav,
        labels=c("1%","5%","95%","99%"),
        method="simple",
        add=T,levels=q,drawlabels = F,
        lty=c("longdash","solid","solid","dashed"))
par(xaxs="i")
plot(ymd(dfsubset$Time,truncated=2L),dfsubset$Value,
     axes=TRUE,
     main="Marine Heatwaves",
     ylab="Cummulative Intensity",
     col="red",pch = 16,
     xlim=c(as.Date("2006-01-01"),as.Date("2022-01-01")),xaxt="n")
box()
axis(1,at = dates,label = format(dates,"%Y"))

#################################################################
# plot_that <- function(index_list,all_group_column,group_title,colnum = 30){
#   qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#   col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#   
#   color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
#   options(repr.plot.width = 8, repr.plot.height =5)
#   df.all.long <- melt(df_freq[ c(index_list,"date","doy_numeric")],id.vars=c("date","doy_numeric"),value.name="biovolume",variable.name="species")
#   ggplot(data=df.all.long,aes(x=doy_numeric))+
#     geom_spline(aes(y=biovolume,colour=species),linewidth=2)+
#     geom_spline(data = df_freq,
#                 aes(x = doy_numeric ,y=.data[[all_group_column]],color="A-All"),linewidth=10,
#                 alpha=0.5)+
#     xlab("Day of Year")+
#     ylab(TeX(r"(Biovolume $(\mu m^{3} mL^{-1}) $)"))+
#     scale_y_continuous(labels = scales::comma,trans="log10")+
#     annotation_logticks(sides="l")+
#     scale_color_manual(values=c("gray",sample(color,colnum)))+
#     ggtitle(paste("Yearly Biovolume Concentration of ",group_title))
#   
# }
# plot_that(top20diatom_index,"diatom","Top 20 Diatoms")
# plot_that(top10dino_index,"dino","Dinoflagellates")


######################
# data<- expand.grid(X=times, Y=log(timescales))
# data$Z <- as.vector(zwav)
# 
# ggplot(data, aes(X, Y, fill= Z)) + 
#   geom_tile()+
#   scale_fill_distiller(palette = "Spectral") +
#   geom_contour(aes(X,Y,z=Z),bins=2)

  

#################################################################
#matrix
pal = colorRampPalette(brewer.pal(9, 'Spectral'))(1024)

sm<-synmat(dat,times,method="ReXWT")
options(repr.plot.width = 6, repr.plot.height =6)
par(mar=c(13,13,2,1)+.1) #mar is margin (bottom,left,top,right)
fields::image.plot(1:length(index),1:length(index),sm,
                   col=pal,axes = F,
                   zlim=c(-1,1),
                   ylab="",xlab="",main="Synchrony Matrix")
axis(2,at=seq(1,length(index),1),label=index,las = 1)
axis(1,at=seq(1,length(index),1),label=index,las = 3)


###################################################################
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


