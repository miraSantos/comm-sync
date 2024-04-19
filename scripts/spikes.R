basepath = "/home/mira/MIT-WHOI/github_repos/comm-sync/"
list.of.packages <- c("dplyr","reshape2","ggplot2","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#FIND SPIKES AND PLOT

y = df_carbonC_wyear_mean$Acanthoica_quattrospina
ThresholdingAlgo <- function(y,lag,threshold,influence) {
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag], na.rm=TRUE)
  stdFilter[lag] <- sd(y[0:lag], na.rm=TRUE)
  abs(y[i]-avgFilter[i-1])
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i], na.rm=TRUE)
    stdFilter[i] <- sd(filteredY[(i-lag):i], na.rm=TRUE)
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}

lag=50
threshold = 250000
influence = 0.2
ThresholdingAlgo(df_carbonC_wyear_mean$Acantharia,lag,threshold,influence)
#species, #concentration #duration

retrieve_index <- function(column,threshold){which(column >= threshold)}

threshold=100000 #ug carbon/ml
df_spikes <- apply(df_carbonC_wyear_mean[,protist_tricho_labelC],2,retrieve_index,threshold=threshold)

ggplot() + geom_point(data=df_carbonC_wyear_mean[c(df_spikes$Tripos),],
aes(x=date,y=Tripos,color="Tripos"))+
  geom_point(data=df_carbonC_wyear_mean[c(df_spikes$Gonyaulax),],aes(x=date,y=Gonyaulax,color="Gonyaulax"))+
  geom_point(data=df_carbonC_wyear_mean[c(df_spikes$Guinardia_delicatula_merged),],aes(x=date,y=Guinardia_delicatula_merged,color="Guinardia Delicatula"))+
  scale_x_date(date_breaks="2 years",date_labels = format("%Y"))+
  xlab("Date") + ylab("Carbon Concentration (ug/mL)")


spike_list <- c("date","Favella","Eucampia","Gonyaulax","Tripos",
                "Thalassionema")
df_max <- df_carbonC_wyear_mean %>% select(all_of(spike_list))%>% 
  gather(species,conc,-date) %>%
  group_by(species) %>%
  summarise(max=max(conc,na.rm=TRUE),max_index = which.max(conc))%>%
  mutate(index_x = case_when(max_index < 200 ~ max_index + 50,
                             max_index > 200 ~ max_index -50))

df_carbonC_wyear_mean %>% select(all_of(spike_list))%>% 
  gather(species,conc,-date) %>%
  ggplot() + 
  geom_line(aes(x=date,y=conc,color=species)) +
  xlab("Date") + ylab(expression("Carbon Concentration ("*mu*"g mL"^-1*")"))+
  scale_x_date(date_breaks="2 years",date_labels=format("%Y"))+
  scale_y_sqrt() + 
  geom_text(data = df_max, aes(x = df_carbonC_wyear_mean$date[df_max$index_x],
                y = df_max$max,
                label = df_max$species,
    color = species),show.legend = FALSE)

ggsave(filename = paste0(basepath,"/figures/spikes_timeline.png"),
       width=1500,height=500,units="px",dpi=150)


