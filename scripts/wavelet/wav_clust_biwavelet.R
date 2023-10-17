 #Loading Packages
list.of.packages <- c("biwavelet","dendextend","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

df<- read.csv("/dos/MIT-WHOI/Week.2023.04.24-30/df_biovolume_interpolated_2023_April_27.csv")

# Perform wavelet clustering on the indices
times<-(seq(1,nrow(df),1))
w.arr=array(NA, dim=c(length(all_index), 120, 5612))
for (i in 1:length(all_index)){
  print(paste0(i," of ",length(all_index)))
  wt.t = wt(cbind(times,df[all_index[i]]))
  w.arr[i,,] = wt.t$wave
}



# Compute dissimilarity and distance matrices
w.arr.dis <- wclust(w.arr)
#generate hierarchical cluster
par(mar=c(5, 4, 1, 1), xpd=TRUE)
h.arr <- hclust(w.arr.dis$dist.mat, method = "ward.D")
h.arr$labels=all_index
pdf(file="/dos/MIT-WHOI/Week.2023.05.15-21/dendrogram.pdf", height=40, width=30)
plot_horiz.dendrogram(as.dendrogram(h.arr), side = TRUE)
dev.off()
#cutting the tree into clusters
h.clusts = cutree(h.arr,k=10)

group_index= names(which(h.clusts == 1))

color<-rainbow(length(group_index))
par(mar=c(9, 4, 4, 20), xpd=TRUE)
plot.ts(df_spline[group_index],plot.type="single",col=color,main="Wavelet Clustering",ylab="(Biovolume^(1/3)")
legend("topright", legend = group_index, col = color, lty = 1,inset=c(-0.8, 0))

