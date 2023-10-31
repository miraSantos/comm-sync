basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("tidyr","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)



nao_wide<- read.table("/home/mira/MIT-WHOI/github_repos/comm-sync/data/climate_indices/norm.nao.monthly.b5001.current.ascii.table.txt",sep="",skip=1)
head(nao_wide)
colnames(nao_wide) <- c("year","Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
nao_long <- melt(nao_wide,id.vars="year",variable.name = "month",value.name = "nao_index")
nao_long$month <- match(nao_long$month,month.abb)
write.csv(nao_long,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/nao_index.csv")





amo_wide<- read.table("/home/mira/MIT-WHOI/github_repos/comm-sync/data/climate_indices/ersst.v5.amo.dat.txt",header=T,skip=1)
head(amo_wide)
colnames(amo_wide) <- c("year","month","amo_index")
write.csv(amo_wide,file="/home/mira/MIT-WHOI/Week.2023.10.15-21/data_to_share/amo_index.csv")



