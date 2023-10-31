if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggplot2)


cor(df_sal_merged$norchan_salt,df_sal_merged$mean_salt)
cor(df_sal_merged$wmg_salt,df_sal_merged$mean_salt)


cor(df_sal_merged$mbay_salt,df_sal_merged$mean_salt,method="pearson")
cor(df_sal_merged$mbay_salt,df_sal_merged$mean_salt,method="kendall")
cor(df_sal_merged$mbay_salt,df_sal_merged$mean_salt,method="spearman")

ggplot(df_sal_merged) + geom_point(aes(x=date,y=mean_salt),size=1,color="darkgreen")+
  geom_point(aes(x=date,y=mbay_salt),color="orange",size=1,alpha=0.3)+
  geom_point(aes(x=date,y=wmg_salt),color="blue",size=1,alpha=0.3)+
  geom_point(aes(x=date,y=norchan_salt),color="darkred",size=1,alpha=0.3)+
  geom_point(aes(x=date,y=jorbas_salt),color="purple",size=1,alpha=0.3)

  
  scale_x_date(date_breaks="2 years",date_labels = "%Y" )

  ggplot(df_sal_merged) + 
  geom_point(aes(x=date,y=jorbas_salt),color="purple",size=1,alpha=0.3)
  


