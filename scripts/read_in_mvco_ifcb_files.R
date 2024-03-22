data_path = paste0(basepath,"data/raw_mvco_ifcb/CSVs/2023_11_MVCO/")
carbonC_list <- list.files(data_path)

df_carbon <- read.csv(paste0(data_path,carbonC_list[1]),header=T)
df_carbon_metadata <- read.csv(paste0(data_path,carbonC_list[5]),header=T)
df_carbon_optthresh <-read.csv(paste0(data_path,carbonC_list[8]))

protist_tricho_label <- read.csv(paste0(data_path,carbonC_list[9]),header=F)$V1
diatom_label <- read.csv(paste0(data_path,carbonC_list[3]),header=F)$V1
dino_label <- read.csv(paste0(data_path,carbonC_list[4]),header=F)$V1
ciliate_label <-read.csv(paste0(data_path,carbonC_list[2]),header=F)$V1
nanoflagcocco_label <- read.csv(paste0(data_path,carbonC_list[7]),header=F)$V1
metazoan_label <- read.csv(paste0(data_path,carbonC_list[6]),header=F)$V1

df_carbon$date <- as.Date(df_carbon_metadata$sample_time, format="%Y-%m-%d %H:%M:%S")

save(df_carbon,file=paste0(basepath,"data/r_objects/unfilled/2023_Mar_15_df_carbon.RData"))
save(df_carbon_metadata,file=paste0(basepath,"data/r_objects/unfilled/2023_Mar_15_df_carbon_metadata.RData"))
save(df_carbon_optthresh,file=paste0(basepath,"data/r_objects/unfilled/2023_Mar_15_df_carbon_optthresh.RData"))
