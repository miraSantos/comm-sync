sublog <- function(x){return(log10(x+0.5*min(x[x>0],na.rm=T)))}
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x,na.rm=na.rm)) /(max(x,na.rm=na.rm)-min(x,na.rm=na.rm)))
  }

Colwells <- function(flow.ts, fn = "mean", boundaries = "transform", s = 11, base.binning = 2, from = 0.5, by = 0.25, base.entropy = 2, indices.only = FALSE) {
  fn <- match.fun(fn)
  
  
  # fn<-summary.stat
  
  flow.ts$week <- factor(strftime(flow.ts[, "Date"], format = "%V"))
  flow.ts$year <- factor(strftime(flow.ts[, "Date"], format = "%Y"))
  
  flow.ts.weekly <- aggregate(Q ~ week + year, flow.ts, fn, na.rm = TRUE)
  
  
  
  if (boundaries == "transform") {
    
    flow.ts.weekly$Q <- log10(flow.ts.weekly$Q + 1)
    flow.ts.weekly$flow.class <- cut(flow.ts.weekly$Q, s, right = FALSE, include.lowest = TRUE)
    flow.table <- with(flow.ts.weekly, table(flow.class, week))
    pbreaks <- "see Table"
    
    
  } else {
    
    if (boundaries == "log_class_size") {
      
      breaks <- base.binning^(seq(1:s) - 2)
      breaks <- c(-Inf, breaks, Inf)
      pbreaks <- breaks
      flow.ts.weekly$flow.class <- cut(flow.ts.weekly$Q, breaks)
      flow.table <- with(flow.ts.weekly, table(flow.class, week))
      
    } else {
      
      if (boundaries == "weighted_log_class_size") {
        low_exp <- ceiling(0 - (s - 1)/2)
        breaks <- base.binning^seq(low_exp, length.out = s - 1)
        breaks <- c(-Inf, breaks, Inf)
        pbreaks <- breaks
        breaks <- fn(flow.ts.weekly$Q) * breaks
        
        
        flow.ts.weekly$flow.class <- cut(flow.ts.weekly$Q, breaks, right = FALSE, include.lowest = TRUE)
        flow.table <- with(flow.ts.weekly, table(flow.class, week))
        
        
      } else {
        
        if (boundaries == "equal") {
          flow.ts.weekly$flow.class <- cut(flow.ts.weekly$Q, s, right = FALSE, include.lowest = TRUE)
          flow.table <- with(flow.ts.weekly, table(flow.class, week))
          pbreaks <- "see Table"
        } else {
          
          if (boundaries == "Gan") {
            Q <- fn(flow.ts.weekly$Q, na.rm = TRUE)
            breaks <- seq(from = from, by = by, length.out = s - 1)
            pbreaks <- breaks
            breaks <- Q * breaks
            breaks <- c(-Inf, breaks, Inf)
            flow.ts.weekly$flow.class <- cut(flow.ts.weekly$Q, breaks, right = FALSE, include.lowest = TRUE)
            flow.table <- with(flow.ts.weekly, table(flow.class, week))
          }
        }
      }
    }
  }
  # X<-margin.table(flow.table, 2)
  X <- apply(flow.table, 2, sum, na.rm = T)
  # Y<-margin.table(flow.table, 1)
  Y <- apply(flow.table, 1, sum, na.rm = T)
  Z <- sum(flow.table, na.rm = TRUE)
  
  HX <- -1 * sum((X/Z) * log(X/Z, base = base.entropy), na.rm = TRUE)
  HY <- -1 * sum((Y/Z) * log(Y/Z, base = base.entropy), na.rm = TRUE)
  HXY <- -1 * sum((flow.table/Z) * log(flow.table/Z, base = base.entropy), na.rm = TRUE)
  
  GM <- 2*Z*(HX + HY - HXY)
  GC <- 2*Z*(log(s)-HY)
  GP <- GM + GC
  
  P <- round(1 - (HXY - HX)/log(s, base = base.binning), 2)
  C <- round(1 - HY/log(s, base = base.binning), 2)
  M <- round((HX + HY - HXY)/log(s, base = base.binning), 2)
  
  
  if (indices.only == TRUE) {
    
    return(data.frame(P = P, C = C, M = M, CP = C/P, MP = M/P))
    
  } else {
    
    return(list(breaks = pbreaks, flow.table = flow.table,
                P = P, C = C, M = M, CP = C/P, MP = M/P,
                GM=GM, GC = GC, GP = GP))
  }
} 
