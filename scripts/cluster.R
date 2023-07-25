#Loading Packages
list.of.packages <- c("RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","stats","factoextra","cluster","dtwclust","dendextend","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#Loading in packages
df <- read.csv("/dos/MIT-WHOI/Week.2023.04.24-30/df_biovolume_interpolated_2023_April_27.csv")


#example sline
doy_spline = spline(x = df$doy_numeric,y = df$Acanthoica_quattrospina^(1/3),xout=seq(1,365,10))

#plotting an example spline
par(mar=c(5, 4, 4, 1), xpd=TRUE)
plot(df$doy_numeric,df$Acanthoica_quattrospina^(1/3))
points(doy_spline$x,doy_spline$y,col="red",pch=16)

#computing splines across all columns
df_spline <- df %>%
  reframe(across(all_index,~spline(x =doy_numeric,y = .x^(1/4),xout=seq(1,365,7))))

values = list()
for (col in all_index){
  values <- append(values,  df_spline[[col]]["y"])
}
df_spline <- data.frame(values)
colnames(df_spline) <- all_index

#Normalizing data
df_spline.scaled <- scale(df_spline,scale=FALSE)

#finding optimal number of clusters
fviz_nbclust(df_spline.scaled,kmeans,k.max=50)

km.res <- kmeans(t(df_spline.scaled),5,nstart = 50)
fviz_cluster(km.res,data=t(df_spline.scaled))

group_index = names(which(km.res$cluster == 4))
color<-rainbow(length(group_index))
par(mar=c(5, 4, 4, 20), xpd=TRUE)
plot.ts(df_spline[group_index],plot.type="single",col=color,main="Cluster 4",ylab="(Cube-transformed Biovolume concentration")
legend("topright", legend = group_index, col = color, lty = 1,inset=c(-0.9, 0))


group4 = names(which(km.res$cluster == 4))
group3 = names(which(km.res$cluster == 3))

#############################################################
# Using Eclust and FANNY CLUSTERING
#############################################################
clust.res = eclust(t(df_spline.scaled),FUNcluster="fanny")
fviz_cluster(clust.res)

group_index = names(which(clust.res$clustering == 4))
color<-rainbow(length(group_index))
par(mar=c(9, 4, 4, 20), xpd=TRUE)
plot.ts(df_spline[group_index],plot.type="single",col=color,main="Fanny clustering Group 4",ylab="(Biomass^(1/3)")
legend("topright", legend = group_index, col = color, lty = 1,inset=c(-0.8, 0))


################################################################
#Dynamic Time Warping
#################################################################
dtw.clust <- tsclust(t(df_spline.scaled),type="partitional",k=5)
plot(dtw.clust,clus=4)
plot(dtw.clust,type="sc")
plot(dtw.clust,type="centroids",clus=1)

pdf(file="/dos/MIT-WHOI/Week.2023.05.15-21/dtw_dendrogram.pdf", height=40, width=30)
plot_horiz.dendrogram(as.dendrogram(dtw.clust), side = TRUE)
dev.off()

group_index= all_index_flagged[which(dtw.clust@cluster == 4),]
color<-rainbow(length(group_index[,1]))
par(mar=c(9, 4, 4, 20), xpd=TRUE)
plot.ts(df_spline[group_index[,1]],plot.type="single",col=color,main="Wavelet Clustering",ylab="(Biovolume^(1/3)")
legend("topright", legend = group_index[,1], col = color, lty = 1,inset=c(-0.8, 0))
legend("topright", legend = group_index[,2], col = color, lty = 1,inset=c(-0.8, 0))

