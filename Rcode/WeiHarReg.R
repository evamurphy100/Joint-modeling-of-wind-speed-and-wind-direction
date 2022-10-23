library(fitdistrplus)

Harmonic <- function(theta, K){
  ################################################################
  ## Computes the cosine and sine values of each angle of interest
  ################################################################  
  #theta - one wind direction value
  #K = number of Fourier series to be used
  t <- outer(theta, 1:K)
  return(cbind(apply(t, 2, cos), apply(t, 2, sin)))
}

bin<- function(WD){
  ############################################################
  ## Bins the data with respect to wind direction
  ############################################################
  # WD - wind direction to be binned
  # Length of a bin
  deg<- 10*pi/180
  # Define the range of each bin
  dir.breaks <- seq(0,  2*pi, deg)
  # Assign each direction to a bin range
  dir.binned <- cut(WD, breaks = dir.breaks, ordered_result = TRUE)
  # Generate labels
  dir.labels <- as.character(c(seq(0, 2*pi-deg, by = deg), 0))
  # replace ranges with bin labels
  levels(dir.binned) <- dir.labels
  # Assign bin names to the original data set
  BIN <- dir.binned
}

WeiHarReg = function(WS, WD, K, p = c(0.5, 0.95)){
  ############################################################
  ## Computes the conditional curves of the directional wind 
  ## speed distribution
  ############################################################
  # WS - wind speed data
  # WD - wind direction data in radians
  # K = number of Fourier series to be used
  # p = quantile level
  # Bin the wind direction data
  WD.bins <- bin(WD)
  WSWD.data <- data.frame(WS, WD, WD.bins)
  # In each bin compute the summary statistics for wind direction
  dir.summary <- aggregate(WSWD.data$WD, by = list(WSWD.data$WD.bins), summary)
  dir.median <- dir.summary$x[,3]
  # In each bin fit the wind speed data to a Weibull distribution and
  # estimate the parameters using MLE
  Weibull.MLE.dir <- aggregate(WSWD.data$WS, by = list(WSWD.data$WD.bins), function(z) fitdist(z[z > 0], "weibull")$estimate)
  Weibull.Se.dir <- aggregate(WSWD.data$WS, by = list(WSWD.data$WD.bins), function(z) fitdist(z[z > 0], "weibull")$sd)
  # Construct the dependence of the Weibull parameter estimates on wind direction
  X.harmonics <- Harmonic(dir.median, K)
  shape.wls<- lm(Weibull.MLE.dir$x[,1] ~ X.harmonics, weights = 1/(Weibull.Se.dir$x[,1] )^2)
  scale.wls<- lm(Weibull.MLE.dir$x[,2] ~ X.harmonics, weights = 1/(Weibull.Se.dir$x[,2])^2 )
  #Compute the conditional quantile curves of the directional wind speed distribution
  yg.shape <- cbind(rep(1, 629), Harmonic(seq(0, 2*pi, by = 0.01), K)) %*% shape.wls$coefficients
  yg.scale <- cbind(rep(1, 629), Harmonic(seq(0, 2*pi, by = 0.01), K)) %*% scale.wls$coefficients
  Weibull.q.wls <- array(dim = c(629, length(p)))
  for (j in 1:length(p)){
    Weibull.q.wls[, j] <- qweibull(p[j], yg.shape, yg.scale)
  }
  return(Weibull.q.wls)
}
