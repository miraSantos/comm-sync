basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("RColorBrewer","lubridate","ggplot2","dplyr","dtwclust","tidyr","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/df_carbon_2024_Mar_24.RData"))

#create function to compute 4th root
quadroot <- function(x){x^(1/4)}

#create columns for day of year and week of year
df_carbonC_filled$doy_numeric <- yday(df_carbonC_filled$date)
df_carbonC_filled$week <- week(df_carbonC_filled$date)

#generate dataframes with day of year and week of year means
doy_means <- df_carbonC_filled %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  group_by(doy_numeric) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)
week_means <- df_carbonC_filled %>% 
  mutate_at(protist_tricho_labelC,quadroot) %>%
  group_by(week) %>%
  summarize_at(protist_tricho_labelC,mean,na.rm=T)

plot(df_spline$Acantharia)
##########################
# using TSClust
##########################

#zscore normalization
#note: each time series must be a row 
series.norm <- zscore(as.data.frame(t(week_means[full_periodicity_list])))
row.names(series.norm) <- full_periodicity_list
head(series.norm)

#partitional clustering
clust.part <- tsclust(series = series.norm, type="partitional",k=3L:15L, 
                      distance="dtw", clustering="pam",seed = 2)

k = 6
k_ind = k-2
plot(clust.part[[k_ind]], type = "sc")

#extract member names from cluster
str(clust.part[[k_ind]]@cluster)
df_species_clust <- data.frame(species = full_periodicity_list,cluster = as.numeric(clust.part[[k_ind]]@cluster))
df_species_clust$species[df_species_clust$cluster==1]


#computing the cluster validity indices
df_cvi <- na.omit(sapply(clust.part,cvi,b=seq(1,length(full_periodicity_list),1)))
colnames(df_cvi) <- as.character(seq(3,15,1))
head(df_cvi)

#plot CVI score across cluster indicies
#see for cvi explanations https://rdrr.io/cran/dtwclust/man/cvi.html
for(i in 1:length(row.names(df_cvi))){
  print(i)
  png(filename=paste0(basepath,"figures/wavelet_transforms/annual_cycle/cluster_scores/","clust_part_score_",
                      rownames(df_cvi)[i],".png"),
      width = 500,height= 400,units="px")
plot(df_cvi[i,],xlab="clusters",ylab=paste(rownames(df_cvi)[i],"Score"),
     main=paste(rownames(df_cvi)[i],"Score"),pch=16,cex=2)
dev.off()
}

## COMPUTE CUMULATIVE SCORE?
#convert the indicies to be minimized to negative
df_cvi_mod = as.data.frame(t(df_cvi)) %>% mutate_at(c("DB","DBstar","COP","VI"),funs(.*-1)) %>% mutate(CH = scale(CH))
rowSums(df_cvi_mod)
plot(rowSums(df_cvi_mod),xlab="Cluster Size",ylab="Cummulative Score")
