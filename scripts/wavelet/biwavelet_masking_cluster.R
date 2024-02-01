list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments","stringr",
                      "dendextend","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
lapply(list.of.packages, require, character.only = TRUE)
source("/dos/MIT-WHOI/community_sychrony/scripts/adv_biwavelet_packages.R")
rm(wt)
rm(wclust)

load("/home/mira/MIT-WHOI/community_sychrony/data/r_objects/2023_Jul_10_dfcarbon_group.RData")

nrows = 79
ncols = 5677

#path to folder with Rdata files for clustering
root_path = "/home/mira/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/wtc_arc_rdata_files/temp/"
#generate list of all RData files for clustering
wt_list = list.files(path = root_path)
#removing 2 groups bc they are full nans
#create matrix to store all 
w.arr <- array(dim = c(length(wt_list), nrows, ncols))
#for loop to load in RData files
for(i in 1:length(wt_list)){
  print(i)
  load(file =paste0(root_path,wt_list[i]))
  #generate significance mask
  sig_mask = (res$signif > 1)

  #apply significance mask to file
  res$phase[!sig_mask] <- 0
  #add to matrix
  w.arr[i,,] <- res$phase
} 

sorted_shared_list = sort(shared_list)
s.arr <- array(dim = c(length(wt_list),ncols))
yearly =seq(from=1, to=ncols, by=365)
for(k in 1:ncols){
  print(k)
  s.arr[,k]<-sorted_shared_list[sort(w.arr[,55,k],index.return=TRUE)$ix]
} 

s.df = as.data.frame(s.arr[1:5,yearly])
s.df$rank = 1:nrow(s.df)

long.dat <-s.df %>%
  gather(key="V", value = "species",-rank) %>%
  mutate(year=as.numeric(str_extract(V, "[0-9]+")))

ggplot(data = long.dat, aes(x = year, y = rank, color = species)) +
  geom_line() +
  scale_y_reverse()+
  scale_fill_brewer(palette="Paired")
#counting 2d matrices with
count_isna <- array(dim=c(length(wt_list),1))
for(i in 1:length(wt_list)){count_isna[i,] = sum(is.na(w.arr[i,,]))}


num_wtc = length(wt_list) #number of clusters to include
#apply clustering algorithm
dfcarbon_group$year = year(dfcarbon_group$date)
clust_index = which(count_isna==0)

for(ii in unique(dfcarbon_group$year)){print(ii)

year_index = which(dfcarbon_group$year == ii)
w.arr.dis <- wclust(w.arr[clust_index,,year_index])


#compute hierarchical cluster
h.arr <- hclust(w.arr.dis$dist.mat, method = "ward.D")
#set species names as labels for cluster
wt_list_labels = gsub("temp_wtc_arc_","",wt_list)
wt_list_labels = gsub(".RData","",wt_list_labels)
h.arr$labels=wt_list_labels
#save dendogram plot as as pdf
pdf(file=paste0("/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/2023_Jul_10_",as.character(ii),"_dendrogram.pdf"), height=30, width=20)
bottom = 5
left = 15
top = 1
right = 1
par(mar=c(bottom,left,top,right))
#plot cluster as dendogram
plot_horiz.dendrogram(as.dendrogram(h.arr))
dev.off()
save(w.arr.dis,file=paste0("/dos/MIT-WHOI/community_sychrony/figures/biwavelet_coherence/",as.character(ii),"_w.arr.dis.RData"))
}

#cutting the tree into clusters
h.clusts = cutree(h.arr,k=2) #k is desired number of groups
table(h.clusts)
h.clusts[h.clusts==1]
h.clusts[h.clusts==2]
