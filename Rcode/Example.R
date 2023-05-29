load("WindData.RData")
# Define the wind speed and wind direction
WS = WSWD_CESAR$z10
WD = convert(WSWD_CESAR$z10.1 * pi/180)
# Decide on the number of Fourier series
K=8
# Estimate the directional wind speed distribution using the Binned Weibul Harmonic Regression method
Dir.WS.BWHR = BWHR(WS, WD, 8)
# Estimate the directional wind speed distribution using the Periodic B-splines Quantile Regression method
Dir.WS.Quant.BPQR = BPQR(WS, WD, df = 18)
#plot the conditional quantiles
col = c("red", "blue")
wdir<- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
p = c(0.5, 0.95)
plot(WD, WS, pch = 16, col = "grey", cex = 0.16, ,xaxt = "n", ylab = "Wind Speed", xlab = "Wind Direction")
axis(1, at = seq(0, 2*pi, pi / 4), labels = wdir)
for(i in 1:length(p)){
  lines(seq(0, 2*pi, len = 629), Dir.WS.Quant[,i], col = col[i] )
  lines(seq(0, 2*pi, len = 629), Dir.WS.Quant.BPQR[,i], col = col[i], lty = 2 )
}
legend("topleft", c( "median", "95%-quantile"), col = col, pch = 20, bty = "n", cex = 0.6)
legend("topright, c("BWHR", "BPQR"), lty = c(1,2)
