list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments",
                      "dendextend","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
lapply(list.of.packages, require, character.only = TRUE)
source("scripts/adv_biwavelet_packages.R")
rm(wt)
rm(wclust)

nrows = 79
ncols = 5676

#path to folder with Rdata files for clustering
root_path = "/vortexfs1/scratch/msantos/comm_sync/results/temp_wtc_arc/"
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
  res$wave[!sig_mask] <- 0
  #add to matrix
  w.arr[i,,] <- res$phase
} 

#counting 2d matrices with 
count_isna <- array(dim=c(length(wt_list),1))
for(i in 1:length(wt_list)){count_isna[i,] = sum(is.na(w.arr[i,,]))}

num_wtc = length(wt_list) #number of clusters to include
#apply clustering algorithm
w.arr.dis <- wclust(w.arr[1:num_wtc,,])

save(w.arr.dis,file="/vortexfs1/scratch/msantos/comm_sync/results/cluster_res_2023_Jul_03.RData")