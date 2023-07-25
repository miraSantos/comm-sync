#Loading Packages
list.of.packages <- c("biwavelet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source("/dos/MIT-WHOI/Week.2023.04.24-30/adv_biwavelet_packages.R")


wt_arc <- function(d, pad = TRUE, dj = 1 / 12,
                   s0 = 2 * dt, # dt will be evaluated later (s0 is a promise)
                   J1 = NULL, max.scale = NULL, mother = "morlet",
                   param = -1, lag1 = NULL, psig.level = c(0.84, 0.88,0.92,0.96),
                   sig.test = 0,asig.level = 0.95,do.sig = TRUE,anrands = 100) {
  
  # mother <- match.arg(tolower(mother), MOTHERS)
  
  # Check data format
  checked <- check.datum(d)
  n.obs <- checked$n.obs
  dt <- checked$dt
  t <- checked$t
  xaxis <- d[, 1]
  d[,2] <- d[, 2] - mean(d[, 2])
  sigma2 <- var(d[,2])
  
  if (is.null(J1)) {
    if (is.null(max.scale)) {
      max.scale <- (n.obs * 0.17) * 2 * dt # automatic maxscale
    }
    J1 <- round(log2(max.scale / s0) / dj)
  }
  
  if (is.null(lag1)) {
    # Get AR(1) coefficients for each time series
    d.ar1 <- arima(d[,2], order = c(1, 0, 0))$coef[1]
    lag1 <- c(d.ar1)
  }
  
  
  wt <- wt(d = d, pad = pad, dj = dj, s0 = s0, J1 = J1,
           max.scale = max.scale, mother = mother, param = param,
           do.sig = FALSE, lag1 = lag1[1])
  
  
  if (do.sig) {
    
    multi_sigpoint <- array(0, c(length(wt$scale),n.obs,length(psig.level)))
    
    for (i in 1:length(psig.level)) {
      pcritlevel <- wt.sig(d = d, dt = dt, scale = wt$scale, sig.test = sig.test,
                           sig.level = psig.level[i], lag1 = lag1, dof = -1,
                           mother = mother, sigma2 = 1,
                           arima.method = arima.method)$signif
      pcritlevel <- matrix(pcritlevel, nrow = length(pcritlevel), ncol = 1) %*% rep(1, n.obs)
      multi_sigpoint[,,i] <- wt$power / (sigma2 * pcritlevel)
    }
    
    multi_arccrit <- get_arccrits_power(lag1 = lag1,pad = pad, dj = dj, mother = mother,
                                        max.scale = max.scale, param = param, s0 = s0,J1 = J1,
                                        psig.level = psig.level,Nnull =  NULL, dt = dt,
                                        asig.level = asig.level,anrands = anrands,
                                        arima.method = arima.method) 
    
    
    multi_sigarc <- array(0,c(nrow(pcritlevel),ncol(pcritlevel),length(psig.level)))
    
    #transform the point-wise significance matrix to the arc-wise significance matrix
    
    for(j in 1:length(psig.level)) {
      multi_sigarc[,,j] <- sigpoint2sigarc(multi_sigpoint[,,j],wt$scale,multi_arccrit[j])
    }
    
    sigarc <- apply(multi_sigarc,c(1,2),mean) 
    
  } else {
    pcritlevel <- NA
    arc_critlevels <- NA
    sigarc <- NA
    multi_sigarc <- NA
    multi_sigpoint <- NA
  }
  
  
  results <- list(coi = wt$coi,
                  wave = wt$wave,
                  power = wt$power,
                  power.corr = wt$power.corr,
                  phase = wt$phase,
                  period = wt$period,
                  scale = wt$scale,
                  dt = dt,
                  t = t,
                  xaxis = xaxis,
                  s0 = s0,
                  dj = dj,
                  sigma2 = sigma2,
                  mother = mother,
                  type = "wt",
                  signif = sigarc,
                  maxscale = max.scale,
                  J1 = J1,
                  arc_critlevels = multi_arccrit)
  
  
  class(results) <- "biwavelet_adv"
  
  return(results)
}
