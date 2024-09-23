basepath = "/vortexfs1/scratch/msantos/shift/"
list.of.packages <- c("RColorBrewer", "lubridate",
                      "ggplot2","tibbletime","dplyr","tidyr","zoo","stringr",
                      "ggsignif","scales","patchwork","
                      
                      pso")

#find new packages and install them. require all packages in list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
lapply(list.of.packages, require, character.only = TRUE)

load(paste0(basepath,"data/r_objects/2024-08-23_df_carbon_labels.RData"))
load(paste0(basepath,"data/r_objects/2024-08-23_df_carbonC_filled_wyear_mean.RData"))
load(paste0(basepath,"data/r_objects/df_stat_opt_thresh.RData"))
source(paste0(basepath,"/scripts/shift_functions.R"))

args = commandArgs(trailingOnly=TRUE)
jj = as.numeric(args[1])
taxa_i = as.character(args[2])
batch = as.numeric(args[3])
print(paste("index=",jj))
print(paste("taxa=",taxa_i))
print(paste("batch=",batch))


batch_list  = seq(1,5,1)
protist_tricho_labelC
grid.config <- expand.grid(protist_tricho_labelC,batch_list)
index(grid.config)
df.config <- data.frame(ArrayTaskID=index(grid.config),taxa=grid.config$Var1,batch=grid.config$Var2)
write.table(df.config,
            file=paste0(basepath,'scripts/cyclicity/config.txt'),
            quote=FALSE, 
            sep='\t',
            row.names=F)
