list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/dos/MIT-WHOI/Week.2023.04.24-30/adv_biwavelet_packages.R")
rm(wt) #replaces wt with the correct biwavelet version (there are 2 versions)


#LOADING IN DATA FRAME
data.path <- "/dos/MIT-WHOI/data/2023/MVCO_syn_euk_conc_2023_Mar.csv"
df.syn <- read.csv(data.path)
head(df.syn)

df.syn$date=as.Date(df.syn$Time_UTC,format="%d-%b-%Y %H:%M:%S")
df.syn$doy_numeric=yday(df.syn$date)
df.syn = na.omit(df.syn)
cols = c("sum_euk_biovol_perml","sum_syn_biovol_perml")

#NAIVE GAP FILLING
df.syn_freq <- df.syn %>% 
  group_by(date) %>%
  summarize(across(cols,mean)) %>%
  #set to daily frequency
  complete(date = seq.Date(min(df.syn$date),max(df.syn$date), by="day")) %>%
  #fill out doy_numeric
  mutate(doy_numeric = yday(date)) %>%
  group_by(doy_numeric)%>%
  #replace nans for living things with yearly mean
  mutate(across(cols,~replace_na(.,mean(.,na.rm=T))))

plot_single_wt_arc<- function(res,title,save_folder,save_name,plot.phase=FALSE){
  x = res
  ncol = 64
  fill.cols = NULL
  xlab = "Time (Years)"
  ylab = "Period (Days)"
  tol = 1
  plot.cb = TRUE
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
  
  
  png(filename=paste0(save_folder,save_name),
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
  
  xlocs <- seq(1,length(x$t),1)
  axis(side=1,at=xlocs,labels=format(df.syn_freq[xlocs],form))
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


time_index = seq(1,nrow(df.syn_freq),1)
dat = as.matrix(cbind(time_index,df.syn_freq$sum_syn_biovol_perml))
res = wt_arc(dat)


#SYN
plot_single_wt_arc(res,
                   title="Wavelet Transform of Synechecoccus from 2006-2021",
                   save_folder="/dos/MIT-WHOI/community_sychrony/figures/",
                   save_name="Syn_wt.png")

#EUKS
dat = as.matrix(cbind(time_index,df.syn_freq$sum_euk_biovol_perml))
res = wt_arc(dat)

plot_single_wt_arc(res,
                   title="Wavelet Transform of Picoeuks from 2006-2021",
                   save_folder="/dos/MIT-WHOI/community_sychrony/figures/",
                   save_name="euk_wt.png")
