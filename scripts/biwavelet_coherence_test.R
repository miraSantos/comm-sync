list.of.packages <- c("biwavelet","RColorBrewer", "lubridate",
                      "fields","ggplot2","tibbletime","dplyr","sets",
                      "reshape2","ggformula","tidyr","moments")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("/dos/MIT-WHOI/community_sychrony/scripts/adv_biwavelet_packages.R")
rm(wt) #replaces wt with the correct biwavelet version (there are 2 versions)

plot_biwavelet_test <- function(res,title,save_folder,save_name,plot.phase=TRUE){
  x = res
  ncol = 64
  fill.cols = NULL
  xlab = "Time"
  ylab = "Period"
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
  
  
  xlocs <- axTicks(1)
  axis(side = 1, at = xlocs)
  xlocs <- seq(1,length(x$t),365)
  axis.locs <- axTicks(2)
  yticklab <- format(2 ^ axis.locs)
  axis(2, at = axis.locs, labels = yticklab)
  
  
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

period = 32
noise = 0.4
phase_shift = period/2
x = seq(0,365,1)
period_shift= seq(1,10,length.out=length(x))

# Anti-correlated ---------------------------------------------------------
phase_shift= 0
sig1 = sin(2*pi/period*x) + rnorm(length(x),mean=0,sd=noise)
sig2 = sin(2*pi/period*(x+phase_shift))+ rnorm(length(sig1),mean=0,sd=noise)

plot(x,sig1,type="solid",
     xlab = "Time",
     ylab="Signal")
lines(x,sig2,col="red")

dat1 = as.matrix(cbind(x,sig1))
dat2 = as.matrix(cbind(x,sig2))
res = wtc_arc(dat1,dat2,
              s0=10,
              max.scale = 365*4,
              asig.level=c(0.99),
              psig.level=c(0.99),anrands=500)

sig_mask = (res$signif > 1)
#apply significance mask to file
res$phase[!sig_mask] <- 0

plot_biwavelet_test(res,
                   title="Coherence Sig1 and Sig2",
                   save_folder="/dos/MIT-WHOI/community_sychrony/figures/testing/",
                   save_name="test_sig1_sig_correlated_asig_99_psig_99_length_365_anrands_500.png",
                   plot.phase=TRUE)

# Correlated --------------------------------------------------------------

phase_shift = 0
sig1 = sin(2*pi/period*x) + rnorm(length(x),mean=0,sd=noise)
sig2 = sin(2*pi/period*(x+phase_shift))+ rnorm(length(sig1),mean=0,sd=noise)

plot(x,sig1,type="solid",
     xlab="Time",ylab="Signal")
lines(x,sig2,col="red")

dat1 = as.matrix(cbind(x,sig1))
dat2 = as.matrix(cbind(x,sig2))
res = wtc_arc(dat1,dat2,
              asig.level=c(0.99),
              psig.level=c(0.99),anrands=500)

plot_biwavelet_test(res,
                    title="Coherence Sig1 and Sig2",
                    save_folder="/dos/MIT-WHOI/community_sychrony/figures/",
                    save_name="test_sig1_sig_correlated_asig_99_psig_99_length_365_anrands_500.png",
                    plot.phase=TRUE)



# Changing Frequency ------------------------------------------------------

period = 16
phase_shift=period/2
period_shift= seq(1,10,length.out=length(x))
sig1 = sin(2*pi/(period*period_shift)^0.6*x) + rnorm(length(x),mean=0,sd=noise)
sig2 = sin(2*pi/(period*period_shift)^0.6*(x+phase_shift)) + rnorm(length(sig1),mean=0,sd=noise)

plot(x,sig1,type="solid",
     xlab="Time",ylab="Signal")
lines(x,sig2,col="red")

dat1 = as.matrix(cbind(x,sig1))
dat2 = as.matrix(cbind(x,sig2))
res = wtc_arc(dat1,dat2,
              asig.level=c(0.99),
              psig.level=c(0.99),anrands=500)

plot_biwavelet_test(res,
                    title="Coherence Sig1 and Sig2",
                    save_folder="/dos/MIT-WHOI/community_sychrony/figures/",
                    save_name="test_sig1_sig_freq_shift_anti_correlated_asig_99_psig_99_length_365_anrands_500.png",
                    plot.phase=TRUE)


# Brief period of synchrony -----------------------------------------------
period=16
phase_shift = 0
sig1 = sin(2*pi/(period*period_shift)^0.6*x) + rnorm(length(x),mean=0,sd=noise)
sig2 = sin(2*pi/(period*period_shift)^0.6*(x+phase_shift)) + rnorm(length(sig1),mean=0,sd=noise)

end1 = 150
sig1[1:end1] = rnorm(end1,mean = 0,sd = 1)
sig2[1:end1] = rnorm(end1,mean = 0,sd = 1)
start1 = 200
sig1[start1:length(sig1)] = rnorm(length(sig1)-start1+1,mean = 0,sd = 1)
sig2[start1:length(sig2)] = rnorm(length(sig1)-start1+1,mean = 0,sd = 1)

plot(x,sig1,type="solid",
     xlab="Time",ylab="Signal")
lines(x,sig2,col="red")


dat1 = as.matrix(cbind(x,sig1))
dat2 = as.matrix(cbind(x,sig2))
res = wtc_arc(dat1,dat2,
              asig.level=c(0.99),
              psig.level=c(0.99),anrands=500)

plot_biwavelet_test(res,
                    title="Coherence Sig1 and Sig2",
                    save_folder="/dos/MIT-WHOI/community_sychrony/figures/",
                    save_name="test_sig1_sig_2_briefer_sync_correlated_1_correlated_asig_99_psig_99_length_365_anrands_500.png",
                    plot.phase=TRUE)


# Dual Signal of synchrony -----------------------------------------------
x = seq(0,500,1)
phase_shift1 = 0
phase_shift2 = 0
noise = 0.2
period1 = 10
period2 = 100
noise1 =  rnorm(length(x),mean=0,sd=noise)
noise2 =  rnorm(length(x),mean=0,sd=noise)

subsig1_1= sin(2*pi/((period1))*x)+10+noise1
subsig1_2= sin(2*pi/((period2))*x)+10+noise1

subsig2_1 = sin(2*pi/((period1))*(x+phase_shift1))+10+noise2
subsig2_2 = sin(2*pi/((period2))*(x+phase_shift2))+10+noise2

sig1 = subsig1_1 + subsig1_2
sig2 = subsig2_1 + subsig2_2

plot(x,sig1,type="solid",
     xlab="Time",ylab="Signal")
lines(x,sig2,col="red")


dat1 = as.matrix(cbind(x,sig1))
dat2 = as.matrix(cbind(x,sig2))
res = wtc_arc(dat1,dat2,
              asig.level=c(0.99),
              psig.level=c(0.99),anrands=500)

plot_biwavelet_test(res,
                    title="Coherence Sig1 and Sig2",
                    save_folder="/dos/MIT-WHOI/community_sychrony/figures/",
                    save_name="test_sig1_sig_2_dual_freq_correlated_1_correlated_asig_99_psig_99_length_365_anrands_500.png",
                    plot.phase=TRUE)


# 2 brief periods of synchrony -----------------------------------------------
x = seq(0,500,1)

noise = 0.2
period1 = 10
period2 = 100
phase_shift1 = period1/2
phase_shift2 = 0
noise1 =  rnorm(length(x),mean=0,sd=noise)
noise2 =  rnorm(length(x),mean=0,sd=noise)

subsig1_1= sin(2*pi/((period1))*x)+noise1
subsig1_2= sin(2*pi/((period2))*x)+noise1

subsig2_1 = sin(2*pi/((period1))*(x+phase_shift1))+noise2
subsig2_2 = sin(2*pi/((period2))*(x+phase_shift2))+noise2


end1 = 150
subsig1_1[1:end1] = rnorm(end1,mean = 0,sd = 1)
subsig2_1[1:end1] = rnorm(end1,mean = 0,sd = 1)
start1 = 300
subsig1_1[start1:length(x)] = rnorm(length(x)-start1+1,mean = 0,sd = 1)
subsig2_1[start1:length(x)] = rnorm(length(x)-start1+1,mean = 0,sd = 1)

# end1 = 350
# subsig1_2[1:end1] = rnorm(end1,mean = 0,sd = 1)
# subsig2_2[1:end1] = rnorm(end1,mean = 0,sd = 1)
# start1 = 450 
# subsig1_2[start1:length(x)] = rnorm(length(x)-start1+1,mean = 0,sd = 1)
# subsig2_2[start1:length(x)] = rnorm(length(x)-start1+1,mean = 0,sd = 1)

sig1 = subsig1_1 + subsig1_2
sig2 = subsig2_1 + subsig2_2

plot(x,sig1,type="solid",
     xlab="Time",ylab="Signal")
lines(x,sig2,col="red")


dat1 = as.matrix(cbind(x,sig1))
dat2 = as.matrix(cbind(x,sig2))
res = wtc_arc(dat1,dat2,
              asig.level=c(0.99),
              psig.level=c(0.99),anrands=500)

plot_biwavelet_test(res,
                    title="Coherence Sig1 and Sig2",
                    save_folder="/dos/MIT-WHOI/community_sychrony/figures/",
                    save_name="test_sig1_sig_2_dual_freq_brief_1_correlated_asig_99_psig_99_length_365_anrands_500.png",
                    plot.phase=TRUE)


