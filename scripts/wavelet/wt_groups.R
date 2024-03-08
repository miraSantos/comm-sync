list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/home/mira/MIT-WHOI/Week.2023.04.24-30/adv_biwavelet_packages.R")
rm(wt)

#######################################################
#purpose: compute wavelet transform of species
group_index = all_index
save_path = "/home/mira/MIT-WHOI/Week.2023.05.15-21/"
group_name="all_index"

list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/home/mira/MIT-WHOI/Week.2023.04.24-30/adv_biwavelet_packages.R")
rm(wt)

url = "https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/biovol_concentration_by_class_time_series_CNN_daily10Jan2022.csv"
df<-read.csv(url(url))

df$date=as.Date(df$date,format="%d-%b-%Y %H:%M:%S")
df$doy_numeric=yday(df$date)

groups = read.csv(url("https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/IFCB_classlist_type.csv"))
head(groups)
df[sapply(df, is.nan)] <- NA
head(df)

#Load in Data and create data frame
#########################################
env_url = "https://raw.githubusercontent.com/miraSantos/gp_plankton_model/master/data/mvco_env_table.csv"

df_env <- read.csv(url(env_url))
df_env$date <- as.Date(df_env$time_local,format="%d-%b-%Y %H:%M:%S")
df_env$doy_numeric=yday(df_env$date)
head(df_env)

df_env=df_env %>% group_by(date) %>%   summarise(across(c(wind_speed,wind_dir,solar,temp_beam,salinity_beam),function(x) mean(x, na.rm=TRUE)),.groups="drop")
head(df_env)

df <- merge(df,df_env,by="date")
env_index= colnames(df_env)[colnames(df_env) != "date"]

create_groups <- function(func_group){
  reference=groups$CNN_classlist[(groups[func_group]==1)]
  index=intersect(colnames(df),reference)
  return(index)
}

diatom_index <- create_groups("Diatom")
dino_index <- create_groups("Dinoflagellate") #TO DO: exclude dinophycaeae
cocco_index <- create_groups("Coccolithophore")
cilia_index <- create_groups("Ciliate")
flagellate_index <- create_groups("flagellate")

all_index=c(diatom_index,dino_index,cocco_index,cilia_index,flagellate_index)

cilia_index = cilia_index[cilia_index != "ciliate"] 


df_freq <- df %>% 
  #set to daily frequency
  complete(date = seq.Date(min(df$date),max(df$date), by="day")) %>%
  #fill out doy_numeric
  mutate(doy_numeric = yday(date)) %>%
  group_by(doy_numeric)%>%
  #replace nans for living things with yearly mean
  mutate(across(all_of(c(all_index,env_index)),~replace_na(.,mean(.,na.rm=T))))
nan_list= sapply(df_freq, function(x) sum(is.na(x)))
hist(nan_list)

df_freq["diatom"] = rowSums(df_freq[diatom_index])
df_freq["cocco"] = rowSums(df_freq[cocco_index])
df_freq["dino"] = rowSums(df_freq[dino_index])
df_freq["cilia"] = rowSums(df_freq[cilia_index])
df_freq["flagellate"] = rowSums(df_freq[flagellate_index])

#creating index markers for all individual live taxa and for all the functional groups
df_freq$all = rowSums(df_freq[all_index],na.rm=TRUE)
all_sum_index = c("diatom","cocco","dino","cilia","flagellate")
df_freq$all_func = rowSums(df_freq[all_sum_index],na.rm=T)

means = colMeans(df_freq[diatom_index][sapply(df_freq[diatom_index], is.numeric)])
top10diatom_index=names(sort(means,decreasing=TRUE))[1:10]
top20diatom_index=names(sort(means,decreasing=TRUE))[1:20]
top10diatom_index_sansg = names(sort(means,decreasing=TRUE))[2:10]

means = colMeans(df_freq[dino_index][sapply(df_freq[dino_index], is.numeric)], na.rm=TRUE)
top10dino_index=names(sort(means,decreasing=TRUE))[1:10]

means = colMeans(df_freq[cilia_index][sapply(df_freq[cilia_index], is.numeric)], na.rm=TRUE)
top10cilia_index=names(sort(means,decreasing=TRUE))[1:10]

means = colMeans(df_freq[flagellate_index][sapply(df_freq[flagellate_index], is.numeric)], na.rm=TRUE)
top10flag_index=names(sort(means,decreasing=TRUE))[1:10]



#WAVELET TRANSFORM
#################################################

n_iter <- length(group_index) # Number of iterations of the loop

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar


time_index = seq(1,nrow(df_freq),1)
dat = as.matrix(cbind(time_index,df_freq[group_index[1]]))
res = wt_arc(dat)

plot.biwavelet_adv(res)

for (i in 1:n_iter){
dat = as.matrix(cbind(time_index,df_freq[group_index[i]]))
title=paste0("Wavelet Transform of ",group_index[i])

plot_wt <-function(dat,title){
#perform wavelet transform and
res = wt_arc(dat)
##################### MANUALLY PLOTTING
x = res
ncol = 64
fill.cols = NULL
xlab = "Time (Years)"
ylab = "Period (Days)"
tol = 1
plot.cb = TRUE
plot.phase = FALSE
type = "power.corr.norm"
plot.coi = TRUE
lwd.coi = 1
col.coi = "white"
lty.coi = 1
alpha.coi = 0.8
plot.sig = TRUE
lwd.sig = 4
col.sig = "black"
lty.sig = 1
bw = FALSE
legend.loc = c(0.8,0.8,1,1)
legend.horiz = FALSE
arrow.len = min(par()$pin[2] / 30,par()$pin[1] / 40)
arrow.lwd = arrow.len * 0.3
arrow.cutoff = 0.8
arrow.col = "black"
xlim = NULL
ylim = NULL
zlim = NULL
xaxt = "s"
yaxt = "s"
form = "%Y"

if (is.null(fill.cols)) {
  if (bw) {
    fill.cols <- c("black", "white")
  } else {
    fill.cols <-c("#00007F", "blue", "#007FFF", "cyan", 
                  "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
  }
}

col.pal <- colorRampPalette(fill.cols)
fill.colors <- col.pal(ncol)

types <- c("power.corr.norm", "power.corr", "power.norm",
           "power", "wavelet", "phase", "timing.err")

type <- match.arg(tolower(type), types)

if (type == "power.corr" | type == "power.corr.norm") {
  if (x$type == "wtc" | x$type == "xwt") {#-------------------------------
    x$power <- x$power.corr
    x$wave <- x$wave.corr
  } else {
    x$power <- x$power.corr
  }
}

if (type == "power.norm" | type == "power.corr.norm") {
  if (x$type == "xwt") {
    zvals <- log2(x$power) / (x$d1.sigma * x$d2.sigma)
    
    if (is.null(zlim)) {
      zlim <- range(c(-1, 1) * max(zvals))
    }
    
    zvals[zvals < zlim[1]] <- zlim[1]
    locs <- pretty(range(zlim), n = 5)
    leg.lab <- 2 ^ locs
    
  } else if (x$type == "wtc" | x$type == "pwtc") { #------------
    zvals <- x$rsq
    zvals[!is.finite(zvals)] <- NA
    if (is.null(zlim)) {
      zlim <- range(zvals, na.rm = TRUE)
    }
    zvals[zvals < zlim[1]] <- zlim[1]
    locs <- pretty(range(zlim), n = 5)
    leg.lab <- locs
  } else {
    zvals <- log2(abs(x$power / x$sigma2))
    if (is.null(zlim)) {
      zlim <- range(c(-1, 1) * max(zvals))
    }
    zvals[zvals < zlim[1]] <- zlim[1]
    locs <- pretty(range(zlim), n = 5)
    leg.lab <- 2 ^ locs
  }
} else if (type == "power" | type == "power.corr") {
  zvals <- log2(x$power)
  if (is.null(zlim)) {
    zlim <- range( c(-1, 1) * max(zvals) )
  }
  zvals[zvals < zlim[1]] <- zlim[1]
  locs <- pretty(range(zlim), n = 5)
  leg.lab <- 2 ^ locs
} else if (type == "wavelet") {
  zvals <- (Re(x$wave))
  if (is.null(zlim)) {
    zlim <- range(zvals)
  }
  locs <- pretty(range(zlim), n = 5)
  leg.lab <- locs
} else if (type == "phase") {
  zvals <- x$phase
  if (is.null(zlim)) {
    zlim <- c(-pi, pi)
  }
  locs <- pretty(range(zlim), n = 5)
  leg.lab <- locs
} else if (type == "timing.err") {
  zvals <- x$timing.err
  if (is.null(zlim)) {
    zlim <- range(zvals)
  }
  locs <- pretty(range(zlim), n = 5)
  leg.lab <- locs
}


if (is.null(xlim)) {
  xlim <- range(x$t)
}

yvals <- log2(x$period)

if (is.null(ylim)) {
  ylim <- range(yvals)
} else {
  ylim <- log2(ylim)
}

png(filename=paste0(save_path,group_name,"/",
                    group_index[i],"_wt.png"),
    width=1000,height=500,res=120, units = "px")

par(mar=c(4,4,2,3))#bottom,left,top,right
image.plot(x$t,
      yvals,
      t(zvals),
      zlim = zlim,
      xlim = xlim,
      # ylim = rev(ylim),
      xlab = xlab,
      ylab = ylab,
      yaxt = "n",
      xaxt = "n",
      col = fill.colors,
      main=title)

xlocs <- seq(1,length(x$t),365)
axis(side=1,at=xlocs,labels=format(df_freq$date[xlocs],form))
axis.locs <- c(2,4,6,log2(365),log2(365*2),log2(365*4))
yticklab <- format(2 ^ axis.locs,dig=1)
yticklab <- c("4","16","64","1yr","2yr","4yr")
axis(2, at = axis.locs, labels = yticklab,las=1)


# COI
if (plot.coi) {
  # polygon(x = c(x$t, rev(x$t)), lty = lty.coi, lwd = lwd.coi,
  #         y = c(log2(x$coi),
  #               rep(max(log2(x$coi), na.rm = TRUE), length(x$coi))),
  #         col = adjustcolor(col.coi, alpha.f = alpha.coi), border = col.coi)
  polygon(x = c(x$t, rev(x$t)), lty = lty.coi, lwd = lwd.coi,
          y = c(log2(x$coi), rep(max(c(log2(x$coi), x$period), na.rm = TRUE),
                                 length(x$coi))),
          col = adjustcolor(col.coi, alpha.f = alpha.coi), border = col.coi)
}

# sig.level contour
if (plot.sig & length(x$signif) > 1) {
  if (x$type %in% c("wt", "xwt")) {
    contour(x$t, yvals, t(x$signif), level = tol, col = col.sig,
            lwd = lwd.sig, add = TRUE, drawlabels = FALSE)
  } else {
    tmp <- x$signif
    
    contour(x$t, yvals, t(tmp), level = tol, col = col.sig, lwd = lwd.sig,
            add = TRUE, drawlabels = FALSE)
  }
}

# Plot phases
if (plot.phase) {
  a <- x$phase
  
  #-----------------------------------------------------------------------------
  #change was made here
  # Remove phases where power is weak
  if (!is.null(x$type)) {
    if (x$type %in% c("wt", "xwt")) {
      locs.phases <- which(x$signif <= arrow.cutoff)
    } else if (x$type %in% c("wtc", "pwtc")) {
      #v <- x$rsq / x$signif #original code
      v <- x$rsq
      
      locs.phases <- which(v <= arrow.cutoff)
    }
  } else {
    locs.phases <- which(zvals < quantile(zvals, arrow.cutoff))
  }
  
  a[locs.phases] <- NA
  
  phase.plot(x$t, log2(x$period), phases = a,
             arrow.len = arrow.len,
             arrow.lwd = arrow.lwd,
             arrow.col = arrow.col)
}
dev.off()
}
plot_wt(dat,title)
setTxtProgressBar(pb, i)
}
close(pb)
