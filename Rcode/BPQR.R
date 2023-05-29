library("quantreg")
library("splines")
library("pbs")
BPQR = function(WS, WD, df, p = c(0.5, 0.95){
############################################################
  ## Computes the conditional curves of the directional wind 
  ## speed distribution using the Periodic B-spline Quantile Regression method
############################################################
  # WS - wind speed data
  # WD - wind direction data in radians
  # df = degrees of freedom to use with pbs()
  # p = quantile level
# Define the starting and ending knots for periodic b-spline fitting fitting
boundary_knots<- c(0, 2*pi)
# Define the direction values for prediction
xg.qr = seq(0, 2*pi, 0.01)
# Organize wind direction and wind speed in a data frame
Data.win<- data.frame(WD.qr = WD , WS.qr = WS)
# Fit a quantile regressions with periodic b-splines
QRfit<- rq(WS.qr ~ pbs(WD.qr, df = df, Boundary.knots = boundary_knots), tau = p, data = Data.win)
# Use the fitted model to make prediction
QRpred<- predict(QRfit.win, data.frame(WD.qr = xg.qr))
}
