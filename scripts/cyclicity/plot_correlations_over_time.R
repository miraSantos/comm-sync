################################################
#plot correlation over time for each species
###############################################
for(ii in 1:length(protist_tricho_labelC)){
  print(paste0(ii," of ",length(protist_tricho_labelC)))
  ggplot(data=df_max_xcorr) + geom_point(aes_string(x="year",y=protist_tricho_labelC[ii]))+
    geom_line(aes_string(x="year",y=protist_tricho_labelC[ii]))+
    geom_hline(aes(yintercept=0),color="red")+
    scale_x_continuous(breaks=seq(2006,2023,2))+ylim(-1,1)+
    ylab("Correlation Coefficient") +xlab("Year")+
    ggtitle(protist_tricho_labelC[ii])
  
  ggsave(filename=paste0(basepath,"/figures/cyclic_index/correlation_over_time_quadroot_xcorr/cyclic_correlation_over_time_",protist_tricho_labelC[ii],".png"),
         width=600,height=500,units="px",dpi=120)
  
}

