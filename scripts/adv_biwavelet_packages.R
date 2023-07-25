
#####################################################################################
#                         Determine if contour encloses a hole                      #                                                          
#####################################################################################

check_subset <- function(xcheck,ycheck,xp,yp) {
    
   if(xcheck[1]<max(xp)&&xcheck[1]>min(xp)&&ycheck[1]>min(yp)&&ycheck[1]<max(yp)){
       Itemp = inpolygon(xcheck[1], ycheck[1], xp, yp, boundary = FALSE)
   } else {
       Itemp = 0
    
   }

}

#####################################################################################
# Combine all the coordinates associated with geometrically significant patches into# 
# single vector.                                                                    #                                                            
#####################################################################################

compile_cords <- function(C,scales,areacrit) {

    xcords <- NaN
    ycords <- NaN
    normareat <- NaN

    for(j in 1:length(C)) {
   	      
           xtemp <- C[[j]]$x
  		  ytemp <- C[[j]]$y
          
           mscale <- scales[round(mean(ytemp),0)]
           Avector <- abs(polyarea(xtemp,scales[ytemp]))
           normarea <- Avector/(mscale^2)
                                
          if(normarea>areacrit) {
            xcords <- c(xcords,xtemp)
            ycords <- c(ycords,ytemp) 

            ytemp[] <- normarea
            normareat <- c(normareat,ytemp)                                               
          }
         
    }#end of inner for loop  

    results <- list(xcords = xcords,
                    ycords = ycords,
                    normarea = normareat)
    return(results)
}#end of function 

#####################################################################################
#                   Compute null distribution of Arclength                          #                                                                
#####################################################################################

compute_arc_nulldis <- function(sig,wscales) {
#calculates null distribution for arc-wise test
 numscales <- nrow(sig)
 mlength <- matrix(NaN,nrow = numscales, ncol = 1)
 arcvector <- -99

 for(i in 1:numscales) {
	
    stats <- get_sigarcs(sig[i,])
    numevents <- nrow(stats)
    arclength <- stats$alength
    arclength <- arclength[arclength>0]

    #normalize arc length
    narclength <- arclength/wscales[i]
 
    mlength[i] <- mean(narclength)
    arcvector <- c(arcvector,narclength)
   
 }#end of outer for loop

 arcvector <- arcvector[arcvector>0]
 return(arcvector)
 
}#end function


######################################################################################
#Convert point-wise significance matrix into cumulative area-wise significance       #
#matrix                                                                              #                                                          
######################################################################################

convert_multisig <- function(sigpoint,scale,q){

  d <- dim(sigpoint)
  Nsiglevels <- d[3] #number of point-wise significance levels
  numt <- d[2] #number of time points
  numscale <- d[1]#number of scales

  #pad the original point-wise sig matrix to ensure all contours are closed
  sigpad <- array(0,c(numscale+2,numt+2,Nsiglevels))
  sigpad[2:(numscale+1),2:(numt+1),] <- sigpoint

  #add two more scales that are only slightly different from the first and last original scales.
  #This step is necessary because the sig matrix was padded earlier. 
  scalespad <- 1:numscale+2
  scalespad[1] <- scale[1] - 0.01
  scalespad[numscale+2] <- scale[numscale] + 0.01
  scalespad[2:(numscale+1)] <- scale

 multi_siggeo <- array(0,c(nrow(sigpad),ncol(sigpad),Nsiglevels))
  
 for(i in 1:Nsiglevels) { 
          sigtemp <- convert_sig(sigpad[,,i],q[i],scalespad)
          multi_siggeo[,,i] <- sigtemp
 }#end of outer for loop

  sigarea <- apply(multi_siggeo,c(1,2),mean)
  sigarea[sigpad[,,1]<1] <- 0 
  sigarea <- sigarea[2:(numscale+1),2:(numt+1)]

  return(sigarea)
  
}# end of function


#####################################################################################
#    Convert point-wise significance matrix into geometric significance matrix      #                                                                               
#####################################################################################
convert_sig <- function(sigpoint,q,scales,C){
   
  Nscales <- nrow(sigpoint)
   
  #extract the point-wise significance contours   
  C <- contourLines(1:ncol(sigpoint),1:nrow(sigpoint),t(sigpoint), level = 1)
         
  #Create the default geometric significance matrix
  siggeo <- sigpoint
  siggeo[,] <- 0

   if(length(C) > 0) {
       #get the coordinates of all geometrically significant patches
       Ccords <- compile_cords(C,scales,q) 
  
       xcords <- Ccords$xcords
       ycords <- Ccords$ycords
       normarea <- Ccords$normarea
     
       for(k in 2:length(xcords)) {
         siggeo[ycords[k],xcords[k]] <- 2
       }
     
     sig_nohole <- fill_sigholes(C,sigpoint)
     siggeo <- cords2matrix(siggeo,sig_nohole)
     
  }

  return(siggeo)

}#end of function 


#####################################################################################
#                                                                                   #
#####################################################################################

cords2matrix <- function(siggeo,sig_nohole) {

Nscales <- nrow(sig_nohole)

for(i in 1:Nscales) {
          
       stats <- get_sigarcs(sig_nohole[i,])
   
       #indices indicating when an arc begins. Length of btime is the number of arcs. 
       btime <- stats$btime

       #indices indicating when an arc ends
       etime <- stats$etime

       #arc length
       alength <- stats$alength

       #negative value of alength[1] means no point-wise significance arcs were found
       if(alength[1] > 0) {
		
         for(j in 1:length(alength)) {
   		 
           id <- btime[j]:etime[j]
           mratio <- max(siggeo[i,id])	
           
           if(mratio>1){
     	     #fill interior of geometrically significant point-wise significance patch
              siggeo[i,id] <- 2
           }              
   		  	
        }
      }
}#end of outer for loop		

  
 return(siggeo)

}#end of function

#####################################################################################
#           fill holes in the point-wise significance matrix                        #                                                                        
#####################################################################################

fill_sigholes <- function(C,sigpoint){

  for(i in 1:length(C)) {
	
    ycheck <- C[[i]]$y
	xcheck <- C[[i]]$x

   		for(j in 1:length(C)) {
           	yp <- C[[j]]$y
             xp <- C[[j]]$x

   		    Itemp <- check_subset(xcheck,ycheck,xp,yp)           
                 if(Itemp == 1) {
                     #the j-th polygon with cords (xp,yp) has a hole
                     #with cords (xcheck, ycheck)
                     ytemp <- min(ycheck):max(ycheck)
                     xtemp <- min(xcheck):max(xcheck)
                     sigpoint[ytemp,xtemp] <- 1
                     #break because a hole with cords (xcheck,ycheck)
                     # can only belong to one polygon. Exit inner for loop.
                     break
                 }                        		           
           		    
   	  }# end of inner for loop

  }#end of outer for loop


return(sigpoint)

}#end of function


#####################################################################################
#                   Generate Realization of Noise Process                           #                                                            
#####################################################################################
generate_red <- function(d1,N,lag1) {

 get_minroots <- function(ar) {
  		min(Mod(polyroot(c(1, -ar))))
 }#end of helper function 

 ar1_ma0_sim <- function(minroots, ar, n) {

      if (minroots <= 1) {
           stop("'ar' part of model is not stationary")
      }

     nstart <- 2 + ceiling(6 / log(minroots))

     x <- ts(data = rnorm(n + nstart), start = 1 - nstart)
     x <- stats::filter(x, ar, method = "recursive")
     x[-seq_len(nstart)]
     # maybe also this: as.ts(x)
 }#end of helper function

  
 if (is.null(lag1)) {
    # Get AR(1) coefficients for each time series
    d1.ar1 <- arima(d1[,2], order = c(1, 0, 0))$coef[1]
    lag1 <- c(d1.ar1)
 }

 ntimesteps <- length(d1[,1])
 mr1 <- get_minroots(lag1[1])
 ntseq <- seq_len(ntimesteps)

 # Generate time series
 d1 <- cbind(ntseq, ar1_ma0_sim(mr1, lag1[1], ntimesteps))
 return(d1)

}# end of function

#####################################################################################
#                   Get Critical Levels Associated with the Arc-wise Test           #                                                                
#####################################################################################

get_arccrits_coher <- function(lag1 = lag1,pad = pad, dj = dj, mother = mother,
                        max.scale = max.scale, param = param, s0 = s0,J1 = J1,
                     psig.level = psig.level,Nnull =  Nnull, dt = dt,
                      asig.level = asig.level, nrands = 100, anrands = 100) {

  Nsiglevels <- length(psig.level)
  multi_arccrit <- matrix(0,nrow = Nsiglevels,1)

 	
  if (is.null(Nnull)) {   
      Nnull <- 50
  }
  
  pcritlevels <- wtc.multisigs(nrands = nrands, lag1 = lag1,
                      dt = dt, ntimesteps = Nnull, pad = pad, dj = dj, J1 = J1,
                      s0 = s0, max.scale = max.scale, mother = mother,
                      psig.level = psig.level, quiet = TRUE)
   
 for(j in 1:Nsiglevels) {
   #reset the value for each point-wise significance level iteration 
   normarcc <- -1

   while(length(normarcc)< anrands) {

        # Compute the null distribution 
         dnull <- cbind(1:Nnull, rnorm(Nnull))
         dnulla <- generate_red(dnull,Nnull,lag1 = lag1[1])  

         dnull <- cbind(1:Nnull, rnorm(Nnull))
         dnullb <- generate_red(dnull,Nnull,lag1 = lag1[2])  

         wtcnull <- wtc(d1 = dnulla,d2 = dnullb, pad = pad, dj = dj, s0 = s0, J1 = J1,
            max.scale = max.scale, mother = mother, param = param,
            nrands = 0, lag1 = lag1)
 
         rsqnull <- wtcnull$rsq

         if(Nsiglevels==1) {
         	
           null_arc <- compute_arc_nulldis(rsqnull/pcritlevels[,,1],wtcnull$scale)
         } else {
           
           null_arc <- compute_arc_nulldis(rsqnull/pcritlevels[,,j],wtcnull$scale)

         }

         normarcc <- c(normarcc,null_arc)
   }#end while loop

    multi_arccrit[j] <- quantile(normarcc[normarcc>0],asig.level)
 }

 return(multi_arccrit)
}#end of function



#####################################################################################
#                Get Critical Levels Associated with the Arc-wise Test              #                                                                   
#####################################################################################

get_arccrits_power <- function(pad = TRUE, dt = dt, dj = 1 / 12, s0 = 2 * dt,
               J1 = J1, max.scale = NULL, mother = mother,
               param = -1, lag1 = NULL,psig.level = psig.level,
               Nnull = NULL,asig.level = 0.95, anrands = 100,
               arima.method = arima.method) {

 Nsiglevels <- length(psig.level)

 multi_arccrit <- matrix(0,nrow = Nsiglevels,1)

 if (is.null(Nnull)) {
   Nnull <- 50
 }  

 # Compute the null distribution 
 dnull <- cbind(1:Nnull, rnorm(Nnull))
 dnull <- generate_red(dnull,Nnull,lag1 = lag1)  
 wtnull <- wt(d = dnull, mother = mother, param = param,
            do.sig = FALSE, lag1 = lag1[1])
 pcritlevels <- array(0, c(length(wtnull$scale),Nnull,Nsiglevels))
   
 for(k in 1:Nsiglevels) {
      sigtemp <- wt.sig(d = dnull, dt = wtnull$dt, scale = wtnull$scale,
                     sig.level = psig.level[k], lag1 = lag1, dof = -1,
                     mother = mother, sigma2 = 1,
                     arima.method = arima.method)$signif
 
      sigtemp <- matrix(sigtemp, nrow = length(sigtemp), ncol = 1) %*% rep(1, Nnull)
      pcritlevels[,,k] <- sigtemp
 }
  

 for(j in 1:Nsiglevels) {
  
   #reset for each pointwise significance level  
   normarcc <- -99

   while(length(normarcc) < anrands) {
   # Compute the null distribution 
      dnull <- cbind(1:Nnull, rnorm(Nnull))
      dnull <- generate_red(dnull,Nnull,lag1 = lag1)  
 
      wtnull <- wt(d = dnull, mother = mother, param = param,
            do.sig = FALSE, lag1 = lag1[1])
    
      sigma2 <- var(dnull[,2])
    
      if(Nsiglevels==1) {
           signull <- wtnull$power/(sigma2 * pcritlevels[,,1])
           null_arc <- compute_arc_nulldis(signull,wtnull$scale)
      } else {
           signull <- wtnull$power/(sigma2 * pcritlevels[,,j])
           null_arc <- compute_arc_nulldis(signull,wtnull$scale)

      }

     normarcc <- c(normarcc,null_arc)
   
   }#end of while loop
   
    multi_arccrit[j] <- quantile(normarcc,asig.level)

 }#end of for loop

  return(multi_arccrit)

}#end of function

#####################################################################################
#     Calculate Critical Areas of the Cumulative Area-wise Test For coherence       #                                            
#####################################################################################

get_areacrits_coher <- function(pad = TRUE,dj = 1 / 12, mother = "morlet",
               param = -1, lag1 = NULL,psig.level = psig.level,
               Nnull = 100,csig.level = 0.95, anrands = 100,
               nrands = 100, quiet = TRUE) {

 Nsiglevels <- length(psig.level)

 multi_areacrit <- matrix(0,nrow = Nsiglevels,1)

 if (is.null(Nnull)) {
   Nnull <- 50
 } 
 
 dnull <- cbind(1:Nnull, rnorm(Nnull))
 dnull <- generate_red(dnull,Nnull,lag1 = lag1[1]) 

 wtnull <- wt(d = dnull, mother = mother, param = param,
                 do.sig = FALSE, lag1 = lag1[1], max.scale = NULL)

 pcritlevels <- wtc.multisigs(nrands = nrands, J1 = wtnull$J1,s0 = wtnull$s0,
                 dt = wtnull$dt, lag1 = lag1,dj = wtnull$dj,max.scale = NULL,
                 ntimesteps = Nnull, mother = mother,psig.level = psig.level,
                 quiet = quiet) 

 for(j in 1:Nsiglevels) {
   
   #reset the value for each point-wise significance level
   normareac <- -99

   while(length(normareac)<anrands) {

        # Compute the null distribution 
         dnull <- cbind(1:Nnull, rnorm(Nnull))
         dnulla <- generate_red(dnull,Nnull,lag1 = lag1[1])  

         dnull <- cbind(1:Nnull, rnorm(Nnull))
         dnullb <- generate_red(dnull,Nnull,lag1 = lag1[2])  

         wtcnull <- wtc(d1 = dnulla,d2 = dnullb,mother = mother, param = param,
            nrands = 0, lag1 = lag1,max.scale = NULL)
 
         rsqnull <- wtcnull$rsq

         if(Nsiglevels==1) {
         	nullstats <- get_areanull(rsqnull/pcritlevels[,,1],wtcnull$scale)
         } else {
             nullstats <- get_areanull(rsqnull/pcritlevels[,,j],wtcnull$scale)
         }
         
         
         normarea <- nullstats$normarea
        
        if(min(normarea) > 0){
            #a negative value in get_areanull indicates
             #there are no pointwise significance patches
    	      normareac <- c(normareac,normarea)
        }      

   }#end while loop

    multi_areacrit[j] <- quantile(normareac[normareac>0],csig.level)
 }

  return(multi_areacrit)

}#end of function


#####################################################################################
#     Calculate Critical Areas of the Cumulative Area-wise Test For Wavelet Power   #                                                                   
#####################################################################################

get_areacrits_power <- function(pad = TRUE, dt = dt, dj = 1 / 12,
               mother = "morlet",param = -1, lag1 = lag1,
               psig.level = psig.level,Nnull = 100,csig.level = 0.95, 
               anrands = 100,arima.method = arima.method) {

 Nsiglevels <- length(psig.level)

 multi_areacrit <- matrix(0,nrow = Nsiglevels,1)
  
 if (is.null(Nnull)) {
   Nnull <- 50
 } 

 # Compute the null distribution 
 dnull <- cbind(1:Nnull, rnorm(Nnull))
 dnull <- generate_red(dnull,Nnull,lag1 = lag1)
  
 wtnull <- wt(d = dnull, mother = mother, param = param,
            do.sig = FALSE, lag1 = lag1[1])

 pcritlevels <- array(0, c(length(wtnull$scale),Nnull,Nsiglevels))
   
 for(k in 1:Nsiglevels) {
      sigtemp <- wt.sig(d = dnull, dt = wtnull$dt, scale = wtnull$scale,
                     sig.level = psig.level[k], lag1 = lag1, dof = -1,
                     mother = mother, sigma2 = 1,
                     arima.method = arima.method)$signif
 
      sigtemp <- matrix(sigtemp, nrow = length(sigtemp), ncol = 1) %*% rep(1, Nnull)
      pcritlevels[,,k] <- sigtemp
 }
  
 for(j in 1:Nsiglevels) {
   
     normareac <- -99
 
     while(length(normareac) < anrands) {
           # Compute the null distribution 
         dnull <- cbind(1:Nnull, rnorm(Nnull))
         dnull <- generate_red(dnull,Nnull,lag1 = lag1)  
 
         wtnull <- wt(d = dnull, mother = mother, param = param,
            do.sig = FALSE, lag1 = lag1[1])

         sigma2 <- var(dnull[,2])
      
         if(Nsiglevels==1) {
             signull <- wtnull$power/(sigma2 * pcritlevels[,,1])
             nullstats <- get_areanull(signull,wtnull$scale)
         } else {
            signull <- wtnull$power/(sigma2 * pcritlevels[,,j])
            nullstats <- get_areanull(signull,wtnull$scale)
         }

         normarea <- nullstats$normarea

         if(min(normarea) > 0){
           #no pointwise significance patches were found 
           #in this iteration.normarea was set to a negative value
           #in get_areanull to indicate
           #this situation 
    	     normareac <- c(normareac,normarea)
         }
     }#end of inner while loop
    
     multi_areacrit[j] <- quantile(normareac[normareac>0],csig.level)

 }#end of outer for loop

  return(multi_areacrit)

}#end of function

#####################################################################################
#                         Calculates Null Distribution of Normalized Area           #                         
#####################################################################################
get_areanull <- function(signull,scales) {

  #calculates area of patches but ignores holes because there
  #are so few of them for red-noise processes. 

   numt <- ncol(signull)
   numscale <- nrow(signull)
    
   #pad the signif matrix output from wt to ensure all contours are closed
   sigpad <- matrix(0,nrow = numscale+2,ncol = numt+2)
   sigpad[2:(numscale+1),2:(numt+1)] <- signull

   #add two more scales that are only slightly different from the first and last
   #This step is necessary because the sig matrix was padded earlier. 
   scalespad <- 1:numscale+2
   scalespad[1] <- scales[1] -.01
   scalespad[numscale+2]<- scales[numscale] + 0.01
   scalespad[2:(numscale+1)] <- scales

   #extract the coordinates corresponding to the point-wise significance contours
   C <- contourLines(1:(numt+2),scalespad,t(sigpad), level = 1)


  Avector <- matrix(NaN,nrow = length(C), ncol = 1)
  normarea <- matrix(NaN,nrow = length(C), ncol = 1)
  mscale <- matrix(NaN,nrow = length(C), ncol = 1)

  if(length(C)>0) {
       for(i in 1:length(C)) {
	
         ynull <- C[[i]]$y
         xnull <- C[[i]]$x
         mscale[i] <- round(mean(ynull),0)
         Avector[i] <- abs(polyarea(xnull,ynull))
         normarea[i] <- abs(Avector[i]/(mscale[i])^2)  

       }#end of for loop
    } else {
         normarea <- -99
         Avector <- -99
         mscale <- -99
  }

 results <- list(mscale = mscale,
                  normarea = normarea,
                  Avector = Avector)
 return(results)


}#end of function


#####################################################################################
#         Extract Point-wise Significance Arcs from Point-wise Significance Arcs    #                                                               
#####################################################################################

get_sigarcs <- function(sig_atscale) {
	     
   #sig_atscale = a time series of p-values or the ratio between 
   #the wavelet quantity and the test critical level at a particular wavelet scale

	time_indices <- 1:length(sig_atscale)

    #set point-wise insignificant values to NaNs,leaving
    #point-wise significance arcs seperated by NaNs
   	sig_atscale[sig_atscale<1] <- NaN
     
    idarc <- 1 + cumsum( is.na(sig_atscale))
 	not.na <- ! is.na(sig_atscale)
 	result_indices <- split(time_indices[not.na], idarc[not.na])
 	result_arc <- split(sig_atscale[not.na], idarc[not.na])
    
    #The number of point-wise significance indentified at a particular wavelet scale
 	num_arcs <- length(result_arc)

    #The time points when the point-wise significance arcs begin
 	begin_time <- matrix(0,ncol = 1, nrow = num_arcs)

    #The time points when the point-wise significance arcs end
 	end_time <- matrix(0,ncol = 1, nrow = num_arcs)

    #arc length or the number of points composing the arc
 	alength <- matrix(0,ncol = 1, nrow = num_arcs)

    if(num_arcs > 0) {
    
 	 	for(i in 1:num_arcs) {
   			
           arc_values <- result_arc[[i]]
   		  arc_indices <- result_indices[[i]]
           begin_time[i] <- min(arc_indices)
   		  end_time[i] <- max(arc_indices)
   		  alength[i] <- length(arc_values) 
     	}
   
    } else {
          #no point-wise significance arcs detected
          alength <- -99
          btime <- -99
          etime <- -99
    }
    
    results <- list(btime = begin_time,
                     etime = end_time,
                     alength = alength)
   return(results)
}#end of function

#####################################################################################
#                                                                                   #
#####################################################################################

plot.biwavelet_adv <- function(x, ncol = 64, fill.cols = NULL,
                           xlab = "Time", ylab = "Period",
                           tol = 1, plot.cb = FALSE, plot.phase = FALSE,
                           type = "power.corr.norm",
                           plot.coi = TRUE, lwd.coi = 1, col.coi = "white",
                           lty.coi = 1, alpha.coi = 0.5, plot.sig = TRUE,
                           lwd.sig = 4, col.sig = "black", lty.sig = 1,
                           bw = FALSE,
                           legend.loc = NULL,
                           legend.horiz = FALSE,
                           arrow.len = min(par()$pin[2] / 30,
                                           par()$pin[1] / 40),
                           arrow.lwd = arrow.len * 0.3,
                           arrow.cutoff = 0.8,
                           arrow.col = "black",
                           xlim = NULL, ylim = NULL, zlim = NULL,
                           xaxt = "s", yaxt = "s", form = "%Y", ...) {

  if (is.null(fill.cols)) {
    if (bw) {
      fill.cols <- c("black", "white")
    } else {
      fill.cols <- c("#00007F", "blue", "#007FFF",
                     "cyan","#7FFF7F", "yellow",
                     "#FF7F00", "red", "#7F0000")
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

  image(x$t,
        yvals,
        t(zvals),
        zlim = zlim,
        xlim = xlim,
        ylim = rev(ylim),
        xlab = xlab,
        ylab = ylab,
        yaxt = "n",
        xaxt = "n",
        col = fill.colors, ...)

  box()
  if (class(x$xaxis)[1] == "Date" | class(x$xaxis)[1] == "POSIXct") {
    if (xaxt != "n") {
      xlocs <- pretty(x$t) + 1
      axis(side = 1, at = xlocs, labels = format(x$xaxis[xlocs], form, ...))
    }
  } else {
    if (xaxt != "n") {
      xlocs <- axTicks(1)
      axis(side = 1, at = xlocs, ...)
    }
  }

  if (yaxt != "n") {
    axis.locs <- axTicks(2)
    yticklab <- format(2 ^ axis.locs) #, dig = 1)
    axis(2, at = axis.locs, labels = yticklab, ...)
  }

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
  box()

  
  ## Add color bar: this must happen after everything, otherwise chaos ensues!
  if (plot.cb) {
    image.plot(x$t,
               yvals,
               t(zvals),
               zlim = zlim,
               ylim = rev(range(yvals)),
               xlab = xlab,
               ylab = ylab,
               col = fill.colors,
               smallplot = legend.loc,
               horizontal = legend.horiz,
               legend.only = TRUE,
               axis.args =
                 list(at = locs, labels = format(leg.lab, dig = 2),...),
               xpd = NA)
    
  }
}


#####################################################################################
#  Transform the point-wise Significance Matrix to the Arc-wise Significance Matrix #                                                                    
#####################################################################################

sigpoint2sigarc <- function(sigpoint,wscales,arc_critlevel) {
 
 #convert from pointwise significance to arcwise significance
 sigarc <- matrix(0, nrow = nrow(sigpoint),ncol = ncol(sigpoint))
 numscales <- nrow(sigpoint)

 for(i in 1:numscales) {
	
    stats <- get_sigarcs(sigpoint[i,])
   
    btime <- stats$btime
    etime <- stats$etime
    alength <- stats$alength

    #normalize the arc length
    narclength <- alength/wscales[i]

    if(alength[1] > 0) {
      
		for(j in 1:length(alength)) {
   			
     	     sigarc[i,btime[j]:etime[j]] <- narclength[j]/arc_critlevel 			  
		}

    }

      
 }#end of for loop

 return(sigarc)
 
 
}#end of function



#####################################################################################
#                Performs Cumulative Arc-wise Test for Wavelet Power                #                                                          
#####################################################################################

wt_arc <- function(d, pad = TRUE, dj = 1 / 12,
                s0 = 2 * dt, # dt will be evaluated later (s0 is a promise)
                J1 = NULL, max.scale = NULL, mother = "morlet",
                param = -1, lag1 = NULL, psig.level = c(0.84, 0.88,0.92,0.96),
                sig.test = 0,asig.level = 0.95,do.sig = TRUE,anrands = 100) {

   mother <- match.arg(tolower(mother), MOTHERS)

  # Check data format
  checked <- check.datum(d)
  n.obs <- checked$n.obs
  dt <- checked$dt
  t <- checked$t
  xaxis <- d[, 1]
  x <- d[, 2] - mean(d[, 2])
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

}# end of function

#####################################################################################
#                    Performs Cumulative Area-wise Test for wavelet power    #                                                 
#####################################################################################
wt_area <- function(d, pad = TRUE, dt = NULL, dj = 1 / 12, s0 = 2 * dt,
               J1 = NULL, max.scale = NULL, mother = "morlet",
               param = -1, lag1 = NULL, sig.level = 0.95,
               sig.test = 0,do.sig = TRUE, arima.method = "CSS", 
               psig.level = c(0.82,0.86,0.90,0.94,0.98),csig.level = 0.95, 
               anrands = 100,Nnull = 200) {
  
# Check data format
  checked <- check.datum(d)
  n.obs <- checked$n.obs
  dt <- checked$dt
  t <- checked$t
  xaxis <- d[, 1]
  x <- d[, 2] - mean(d[, 2])
  sigma2 <- var(d[,2])

  if (is.null(J1)) {
    if (is.null(max.scale)) {
      max.scale <- n.obs * 0.34 * dt # automaxscale
    }
    J1 <- round(log2(max.scale / s0) / dj)
  }

  if (is.null(lag1)) {
    # Get AR(1) coefficients for each time series
    d.ar1 <- arima(d[,2], order = c(1, 0, 0))$coef[1]
    lag1 <- c(d.ar1)
  }
   
  # This could be made more efficient by removing the +1
  # but this can lead to insufficient padding in some instances.
  # Currently the padding is the same as that of Torrence & Compo (1998)
  if (pad) {
    x <- c(x, rep(0, 2 ^ ceiling(log2(n.obs) + 1) - n.obs))
  }

  n <- NROW(x)
  k <- seq_len(floor(.5 * n))
  k <- k * 2 * pi / (n * dt)
  k <- c(0, k, -k[ floor( .5 * (n - 1) ):1 ])
  f <- fft(x)
  invflen <- 1 / length(f)
  scale <- s0 * 2 ^ ((0:J1) * dj)
  wave <- matrix(0, nrow = J1 + 1, ncol = n)

  for (a1 in seq_len(J1 + 1)) {
    wb <- wt.bases(mother, k, scale[a1], param)
    wave[a1, ] <- fft(f * wb$daughter, inverse = TRUE) * invflen
  }

  period <- wb$fourier.factor * scale
  coi <- wb$coi * dt * c(1e-5,
                         seq_len(.5 * (n.obs + 1) - 1),
                         floor(.5 * n.obs - 1):1,
                         1e-5)

  wave <- wave[, seq_len(n.obs)] ## Get rid of padding before returning
  power.corr <- (abs(wave) ^ 2 * max.scale) /
                matrix(rep(period, length(t)), nrow = NROW(period))

  power <- abs(wave) ^ 2

  phase <- atan2(Im(wave), Re(wave))

  if (do.sig) {
    
    multi_sigpoint <- array(0, c(length(scale),n.obs,length(psig.level)))
    
    for (i in 1:length(psig.level)) {
          pcritlevel <- wt.sig(d = d, dt = dt, scale = scale, sig.test = sig.test,
                     sig.level = psig.level[i], lag1 = lag1, dof = -1,
                     mother = mother, sigma2 = 1,
                     arima.method = arima.method)$signif
          pcritlevel <- matrix(pcritlevel, nrow = length(pcritlevel), ncol = 1) %*% rep(1, n.obs)
          multi_sigpoint[,,i] <- power / (sigma2 * pcritlevel)
     }
  
     #ensemble mean approach
     multi_areacrit <- get_areacrits_power(lag1 = lag1,psig.level = psig.level,
                    csig.level = csig.level, anrands = anrands, Nnull = Nnull,
                    arima.method = arima.method, mother = mother)

     multi_sigpoint[multi_sigpoint < 1] <- 0
     multi_sigpoint[multi_sigpoint >= 1] <- 1

     sigarea <- convert_multisig(multi_sigpoint,scale,multi_areacrit)
                                  
  } else {
     pcritlevel <- NA
     sigpoint <- NA
     multi_areacrit <- NA
     sigarea <- NA
     multi_sigpoint <- NA
  }

  
   results <- list(coi = coi,
                  wave = wave,
                  power = power,
                  power.corr = power.corr,
                  phase = phase,
                  period = period,
                  scale = scale,
                  dt = dt,
                  t = t,
                  xaxis = xaxis,
                  s0 = s0,
                  dj = dj,
                  sigma2 = sigma2,
                  mother = mother,
                  type = "wt",
                  signif = sigarea,
                  maxscale = max.scale,
                  J1 = J1,
                  areacrits = multi_areacrit)
                  
  class(results) <- "biwavelet_adv"
  return(results)
}


#####################################################################################
#    Cumulative Arc-wise Test for Local Wavelet Coherence                           #                                                                                                        
#####################################################################################

wtc_arc <- function(d1, d2, pad = TRUE, dj = 1 / 12,
                s0 = 2 * dt, # dt will be evaluated later (s0 is a promise)
                J1 = NULL, max.scale = NULL, mother = "morlet",
                param = -1, lag1 = NULL, asig.level = 0.95,
                psig.level = c(0.92,0.95),
                nrands = 300, quiet = FALSE, anrands = 300) {

  mother <- match.arg(tolower(mother), MOTHERS)

  # Check data format
  checked <- check.data(y = d1, x1 = d2)
  xaxis <- d1[, 1]
  dt <- checked$y$dt

  t <- checked$y$t
  n <- checked$y$n.obs

  if (is.null(J1)) {
    if (is.null(max.scale)) {
      max.scale <- (n * 0.17) * 2 * dt # automatic maxscale
    }
    J1 <- round(log2(max.scale / s0) / dj)
  }

  if (is.null(lag1)) {
    # Get AR(1) coefficients for each time series
    d1.ar1 <- arima(d1[,2], order = c(1, 0, 0))$coef[1]
    d2.ar1 <- arima(d2[,2], order = c(1, 0, 0))$coef[1]
    lag1 <- c(d1.ar1, d2.ar1)
  }

  # Get CWT of each time series
  wt1 <- wt(d = d1, pad = pad, dj = dj, s0 = s0, J1 = J1,
            max.scale = max.scale, mother = mother, param = param,
            do.sig = FALSE, lag1 = lag1[1])

  wt2 <- wt(d = d2, pad = pad, dj = dj, s0 = s0, J1 = J1,
            max.scale = max.scale, mother = mother, param = param, 
            do.sig = FALSE, lag1 = lag1[2])

  # Standard deviation for each time series
  d1.sigma <- sd(d1[,2], na.rm = T)
  d2.sigma <- sd(d2[,2], na.rm = T)

  s.inv <- 1 / t(wt1$scale)
  s.inv <- matrix(rep(s.inv, n), nrow = NROW(wt1$wave))

  smooth.wt1 <- smooth.wavelet(s.inv * (abs(wt1$wave) ^ 2), dt, dj, wt1$scale)
  smooth.wt2 <- smooth.wavelet(s.inv * (abs(wt2$wave) ^ 2), dt, dj, wt2$scale)
  coi <- pmin(wt1$coi, wt2$coi, na.rm = T)

  # Cross-wavelet computation
  CW <- wt1$wave * Conj(wt2$wave)

  # Bias-corrected cross-wavelet
  CW.corr <- (wt1$wave * Conj(wt2$wave) * max(wt1$period)) /
              matrix(rep(wt1$period, length(t)), nrow = NROW(wt1$period))

  # Power
  power <- abs(CW) ^ 2

  # Bias-corrected power
  power.corr <- (abs(CW) ^ 2 * max.scale) /
                matrix(rep(wt1$period, length(t)), nrow = NROW(wt1$period))

  # Wavelet coherence
  smooth.CW <- smooth.wavelet(s.inv * (CW), dt, dj, wt1$scale)
  rsq <- abs(smooth.CW) ^ 2 / (smooth.wt1 * smooth.wt2)

  # Phase difference
  phase <- atan2(Im(CW), Re(CW))


  if (nrands > 0) {
   
      pcritlevels <- wtc.multisigs(nrands = nrands, lag1 = lag1,
                      dt = dt, ntimesteps = n, pad = pad, dj = dj, J1 = J1,
                      s0 = s0, max.scale = max.scale, mother = mother,
                      psig.level = psig.level, quiet = quiet)

      multi_sigpoint <- array(0,c(nrow(pcritlevels),ncol(pcritlevels),length(psig.level)))
    
      for(j in 1:length(psig.level)) {
            multi_sigpoint[,,j] <- rsq/pcritlevels[,,j]
      }

	  multi_arccrit <- get_arccrits_coher(lag1 = lag1,pad = pad, dj = dj, mother = mother,
                        max.scale = max.scale, param = param, s0 = s0,J1 = J1,
                        psig.level = psig.level, Nnull =  n, dt = dt,
                        asig.level = asig.level, nrands = nrands, anrands = anrands) 

      #transform pointwise significance matrix to arcwise significance matrix
      multi_sigarc <- array(0,c(nrow(pcritlevels),ncol(pcritlevels),length(psig.level)))

      for(j in 1:length(psig.level)) {
    	    multi_sigarc[,,j] <- sigpoint2sigarc(multi_sigpoint[,,j],wt1$scale,multi_arccrit[j])
      }
    
      sigarc <- apply(multi_sigarc,c(1,2),mean) 

  } else {
      pcritlevels <- NA
      multi_arccrit <- NA
      sigarc <- NA
      multi_sigpoint <- NA
  }

  results <- list(coi = coi,
                  wave = CW,
                  wave.corr = CW.corr,
                  power = power,
                  power.corr = power.corr,
                  rsq = rsq,
                  phase = phase,
                  period = wt1$period,
                  scale = wt1$scale,
                  dt = dt,
                  t = t,
                  xaxis = xaxis,
                  s0 = s0,
                  dj = dj,
                  d1.sigma = d1.sigma,
                  d2.sigma = d2.sigma,
                  mother = mother,
                  type = "wtc",
                  signif = sigarc,
                  arc_critlevels = multi_arccrit)

  class(results) <- "biwavelet_adv"
  return(results)
}# end of function 

#####################################################################################
#                  Cumulative Area-wise Test for Local Wavelet Coherence            #                                                         
#####################################################################################

wtc_area <- function(d1, d2, pad = TRUE, dj = 1 / 12,
                s0 = 2 * dt, # dt will be evaluated later (s0 is a promise),
                J1 = NULL, max.scale = NULL, mother = "morlet",
                param = -1, lag1 = NULL, psig.level = c(0.82,0.86,0.90,0.94,0.98),
                nrands = 300, quiet = FALSE, csig.level = 0.95,
                anrands = 100,Nnull = 200) {

  mother <- match.arg(tolower(mother), MOTHERS)

  # Check data format
  checked <- check.data(y = d1, x1 = d2)
  xaxis <- d1[, 1]
  dt <- checked$y$dt

  t <- checked$y$t
  n <- checked$y$n.obs

  if (is.null(J1)) {
    if (is.null(max.scale)) {
      max.scale <- (n * 0.17) * 2 * dt # automatic maxscale
    }
    J1 <- round(log2(max.scale / s0) / dj)
  }

  if (is.null(lag1)) {
    # Get AR(1) coefficients for each time series
    d1.ar1 <- arima(d1[,2], order = c(1, 0, 0))$coef[1]
    d2.ar1 <- arima(d2[,2], order = c(1, 0, 0))$coef[1]
    lag1 <- c(d1.ar1, d2.ar1)
  }

  # Get CWT of each time series
  wt1 <- wt(d = d1, pad = pad, dj = dj, s0 = s0, J1 = J1,
            max.scale = max.scale, mother = mother, param = param, lag1 = lag1[1])

  wt2 <- wt(d = d2, pad = pad, dj = dj, s0 = s0, J1 = J1,
            max.scale = max.scale, mother = mother, param = param, lag1 = lag1[2])

  # Standard deviation for each time series
  d1.sigma <- sd(d1[,2], na.rm = T)
  d2.sigma <- sd(d2[,2], na.rm = T)

  s.inv <- 1 / t(wt1$scale)
  s.inv <- matrix(rep(s.inv, n), nrow = NROW(wt1$wave))

  smooth.wt1 <- smooth.wavelet(s.inv * (abs(wt1$wave) ^ 2), dt, dj, wt1$scale)
  smooth.wt2 <- smooth.wavelet(s.inv * (abs(wt2$wave) ^ 2), dt, dj, wt2$scale)
  coi <- pmin(wt1$coi, wt2$coi, na.rm = T)

  # Cross-wavelet computation
  CW <- wt1$wave * Conj(wt2$wave)

  # Bias-corrected cross-wavelet
  CW.corr <- (wt1$wave * Conj(wt2$wave) * max(wt1$period)) /
              matrix(rep(wt1$period, length(t)), nrow = NROW(wt1$period))

  # Power
  power <- abs(CW) ^ 2

  # Bias-corrected power
  power.corr <- (abs(CW) ^ 2 * max.scale) /
                matrix(rep(wt1$period, length(t)), nrow = NROW(wt1$period))

  # Wavelet coherence
  smooth.CW <- smooth.wavelet(s.inv * (CW), dt, dj, wt1$scale)
  rsq <- abs(smooth.CW) ^ 2 / (smooth.wt1 * smooth.wt2)

  # Phase difference
  phase <- atan2(Im(CW), Re(CW))

  if (nrands > 0) {
    pcritlevels <- wtc.multisigs(nrands = nrands, lag1 = lag1,
                      dt = dt, ntimesteps = n, pad = pad, dj = dj, J1 = J1,
                      s0 = s0, max.scale = max.scale, mother = mother,
                      psig.level = psig.level, quiet = quiet)

    multi_sigpoint <- array(0,c(nrow(pcritlevels),ncol(pcritlevels),length(psig.level)))
    
    for(j in 1:length(psig.level)) {
     	multi_sigpoint[,,j] <- rsq/pcritlevels[,,j]
    }
             
    #ensemble mean method
    multi_areacrit <- get_areacrits_coher(lag1 = lag1, psig.level = psig.level, 
                 Nnull = Nnull, csig.level = csig.level, anrands = anrands)
    
    multi_sigpoint[multi_sigpoint < 1] <- 0
    multi_sigpoint[multi_sigpoint >= 1] <- 1

    sigarea <- convert_multisig(multi_sigpoint,wt1$scale,multi_areacrit)
        
  } else {
    pcritlevels <- NA
    multi_areacrit <- NA
    multi_sigpoint <- NA
    sigarea <- NA
  }

            results <- list(coi = coi,
                  wave = CW,
                  wave.corr = CW.corr,
                  power = power,
                  power.corr = power.corr,
                  rsq = rsq,
                  phase = phase,
                  period = wt1$period,
                  scale = wt1$scale,
                  dt = dt,
                  t = t,
                  xaxis = xaxis,
                  s0 = s0,
                  dj = dj,
                  d1.sigma = d1.sigma,
                  d2.sigma = d2.sigma,
                  mother = mother,
                  type = "wtc",
                  signif = sigarea,
                  areacrits = multi_areacrit)


  class(results) <- "biwavelet_adv"
  return(results)
}

#==================================================================================

#' Determine significance of wavelet coherence
#'
#' @author Tarik C. Gouhier (tarik.gouhier@@gmail.com)
#'
#' Code based on WTC MATLAB package written by Aslak Grinsted.
#'
#' @param nrands Number of Monte Carlo randomizations.
#' @param lag1 Vector containing the AR(1) coefficient of each time series.
#' @param dt Length of a time step.
#' @param ntimesteps Number of time steps in time series.
#' @param pad Pad the values will with zeros to increase the speed of the
#'   transform.
#' @param dj Spacing between successive scales.
#' @param s0 Smallest scale of the wavelet.
#' @param J1 Number of scales - 1.
#' @param max.scale Maximum scale.
#' @param mother Type of mother wavelet function to use. Can be set to
#'   \code{morlet}, \code{dog}, or \code{paul}.
#'   Significance testing is only available for \code{morlet} wavelet.
#' @param sig.level Significance level to compute.
#' @param quiet Do not display progress bar.
#'
#' @return Returns significance matrix containing the \code{sig.level}
#'   percentile of wavelet coherence at each time step and scale.
#'
#' @references
#' Cazelles, B., M. Chavez, D. Berteaux, F. Menard, J. O. Vik, S. Jenouvrier,
#' and N. C. Stenseth. 2008. Wavelet analysis of ecological time series.
#' \emph{Oecologia} 156:287-304.
#'
#' Grinsted, A., J. C. Moore, and S. Jevrejeva. 2004. Application of the cross
#' wavelet transform and wavelet coherence to geophysical time series.
#' \emph{Nonlinear Processes in Geophysics} 11:561-566.
#'
#' Torrence, C., and G. P. Compo. 1998. A Practical Guide to Wavelet Analysis.
#' \emph{Bulletin of the American Meteorological Society} 79:61-78.
#'
#' Torrence, C., and P. J. Webster. 1998. The annual cycle of persistence in the
#' El Nino/Southern Oscillation. \emph{Quarterly Journal of the Royal
#' Meteorological Society} 124:1985-2004.
#'
#' @note The Monte Carlo randomizations can be extremely slow for large
#'   datasets. For instance, 1000 randomizations of a dataset consisting of 1000
#'   samples will take ~30 minutes on a 2.66 GHz dual-core Xeon processor.
#'
#' @examples
#' # Not run: wtcsig <- wtc.sig(nrands, lag1 = c(d1.ar1, d2.ar1), dt,
#' #                            pad, dj, J1, s0, mother = "morlet")
#'
#' @export
wtc.multisigs <- function(nrands = 300, lag1, dt, ntimesteps, pad = TRUE,
                    dj = 1 / 12, s0, J1, max.scale = NULL,
                    mother = "morlet", psig.level = psig.level, 
                    quiet = FALSE) {

  if (nrands < 1) {
    return(NA)
  }

  mr1 <- get_minroots(lag1[1])
  mr2 <- get_minroots(lag1[2])
  ntseq <- seq_len(ntimesteps)

  d1 <- cbind(ntseq, ar1_ma0_sim(mr1, lag1[1], ntimesteps))

  wt1 <- wt(d = d1, pad = pad, dj = dj, dt = dt, s0 = s0, J1 = J1,
           max.scale = max.scale, mother = mother, do.sig = FALSE)

  s.inv <- 1 / t(wt1$scale)
  s.inv <- matrix(rep(s.inv, ntimesteps), nrow = NROW(wt1$wave))

  rand.rsq <- array(dim = c(NROW(wt1$wave), NCOL(wt1$wave), nrands), NA)
  if (!quiet) {
    prog.bar <- txtProgressBar(min = 0, max = nrands, style = 3)
  }

  for (r in seq_len(nrands)) {

    # Generate time series
    d1 <- cbind(ntseq, ar1_ma0_sim(mr1, lag1[1], ntimesteps))
    d2 <- cbind(ntseq, ar1_ma0_sim(mr2, lag1[2], ntimesteps))

    # Wavelet transforms
    wt1 <- wt(d = d1, pad = pad, dj = dj, dt = dt, s0 = s0, J1 = J1,
              max.scale = max.scale, mother = mother, do.sig = FALSE)
    wt2 <- wt(d = d2, pad = pad, dj = dj, dt = dt, s0 = s0, J1 = J1,
              max.scale = max.scale, mother = mother, do.sig = FALSE)

    # Smoothed cross wavelet transform
    smooth.CW <- smooth.wavelet(s.inv * wt1$wave * Conj(wt2$wave),
                                dt, dj, wt1$scale)

    sw1 <- smooth.wavelet(s.inv * (abs(wt1$wave) ^ 2), dt, dj, wt1$scale)
    sw2 <- smooth.wavelet(s.inv * (abs(wt2$wave) ^ 2), dt, dj, wt2$scale)

    rand.rsq[, , r] <- abs(smooth.CW) ^ 2 / (sw1 * sw2)

    if (!quiet) {
      setTxtProgressBar(prog.bar, r)
    }
  }

  if (!quiet) {
    close(prog.bar)
  }

  # The original slow implementation was using "apply" and "quantile" functions
  #result <- apply(rand.rsq,MARGIN = c(1,2), FUN = quantile, probs = psig.level, na.rm = TRUE)
  
  
  #result <- matrix(nrow = nrow(rand.rsq), ncol = ncol(rand.rsq))
  result <- array(0, c(nrow(rand.rsq),ncol(rand.rsq), length(psig.level)))
  
 for(j in 1:length(psig.level)) {
   for (i in seq_len(ncol(rand.rsq))) {
    # TODO: can be facter if we remove as.matrix()
    result[,i,j] <- rcpp_row_quantile(as.matrix(rand.rsq[,i,]), psig.level[j])
   }
 }
  return(result)
}

#' Helper function (not exported)
#' @param ar The 'ar' part of AR(1)
#' @return double
get_minroots <- function(ar) {
  min(Mod(polyroot(c(1, -ar))))
}

#' Slightly faster \code{\link{arima.sim}} implementation which assumes AR(1)
#' and \code{ma=0}.
#'
#' @param minroots Output from \code{\link{get_minroots}} function.
#' @param ar The 'ar' part of AR(1)
#' @param n Length of output series, before un-differencing. A strictly positive
#'   integer.
#' @seealso \code{\link{arima.sim}}
ar1_ma0_sim <- function(minroots, ar, n) {

  if (minroots <= 1) {
    stop("'ar' part of model is not stationary")
  }

  nstart <- 2 + ceiling(6 / log(minroots))

  x <- ts(data = rnorm(n + nstart), start = 1 - nstart)
  x <- stats::filter(x, ar, method = "recursive")
  x[-seq_len(nstart)]
  # maybe also this: as.ts(x)
}


