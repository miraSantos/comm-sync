
basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"

list.of.packages <- c("RColorBrewer", "lubridate","dplyr","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#read in files from read_in_mvco_ifcb_files.R

############# upload quality control
#note df_stat_updated comes from a google sheet called optthresh
opt_thresh <- read.table(paste0(basepath,"data/df_stat_updated.tsv"),sep="\t",header =T)


index_maybe_include = which(opt_thresh$Updated.Status %in% c("maybe","include"))
index_include = which(opt_thresh$Updated.Status =="include")
label_maybe_include <-opt_thresh$Class[index_maybe_include]
label_include <-opt_thresh$Class[index_include]


save(opt_thresh,label_maybe_include,label_include,file=paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))

