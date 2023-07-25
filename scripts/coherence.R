list.of.packages <- c("RColorBrewer","lubridate","fields",
                      "ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","wsyn","tidyr","moments")

#install packages if they aren't already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

env_variable = "solar"
save_path = "/dos/MIT-WHOI/Week.2023.05.22-28/coherence_solar_plots"

sig_matrix <- matrix(nrow=129,ncol=length(all_index),FALSE)

for (ii in 1:length(all_index)){
dat=t(data.matrix(df_freq[all_index[ii]]))
env=t(data.matrix(df_freq[env_variable]))

times = seq(1,nrow(df_freq),1)

env<-cleandat(env,times,1)$cdat
dat<-cleandat(dat,times,1)$cdat

res<-coh(dat1=env,dat2=dat,times=times,norm="powall",
         sigmethod="fast",nrand=100,scale.max.input=1000)
plotmag(res)

sigthresh = c(0.95,0.99)
qs<-apply(X=Mod(res$signif$scoher),FUN=stats::quantile,MARGIN=2,prob=sigthresh)
if (length(sigthresh)==1){qs<-matrix(qs,1,length(qs))}


timescales = res$timescales
coher = res$coher

coh <- data.frame(x = timescales,y = Mod(coher))
coh <- cbind(coh,t(qs))
colnames(coh) <- c("x","y","sig_95","sig_99")

#GENERATE AND SAVE PLOT
colors <- c("Observations"="black", "95% sig" = "red", "99% sig" = "orange")

ggplot(data = coh) + 
  geom_line(aes(x = x, y = y, color="Observations"))+
  geom_line(aes(x=x,y=sig_95, color="95% sig"),lty="dashed")+
  geom_line(aes(x=x,y=sig_99, color = "99% sig"),lty="dotdash")+
  ylim(0,1)+
  annotate('rect', xmin=350, xmax=400, ymin=0, ymax=1, alpha=.2, fill='red')+
  annotate("text",x=580,y=1,label = "350-400 days",col="red")+
  scale_x_continuous(trans="log10")+
  annotation_logticks(sides="b")+
  # scale_x_continuous(breaks = c(-log(1/xlocs),365),labels=c(xlocs,365))+
  xlab("Timescales (Days)")+
  ylab("Coherence")+
  labs(color="Legend")+
  ggtitle(paste("Coherence in",all_index[ii]))

ggsave(paste0(save_path,"/coherence_",all_index[ii],".png"),width=8,height=4,units="in")
  timescales_sig <- coh$y > coh$sig_99
sig_matrix[,ii] <- timescales_sig

print(paste(ii,"of",length(all_index)))
}
#############################################################
ii = 1
dat=t(data.matrix(df_freq[all_index[ii]]))
dat=t(data.matrix(df_freq["Ditylum_brightwellii"]))
env=t(data.matrix(df_freq$temp_beam))

times = seq(1,nrow(df_freq),1)

env<-cleandat(env,times,1)$cdat
dat<-cleandat(dat,times,1)$cdat

res<-coh(dat1=env,dat2=dat,times=times,norm="powall",
         sigmethod="fast",nrand=200,scale.max.input=1000)
plotmag(res)
plotphase(res)

