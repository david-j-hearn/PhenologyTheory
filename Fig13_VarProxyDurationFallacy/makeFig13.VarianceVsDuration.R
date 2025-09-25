source("phenologyDistributions.R")
source("phenologyInference.R")

min = 0
max = 365

mu_D = 50
mu_O = 180
mu_C = mu_O + mu_D

sigma1 = 10
sigma2 = 50

layout(matrix(c(1, 2), nrow = 1, byrow = TRUE))
par(mar = c(4, 4, 2, 3))

x = seq(min,max,0.1)

E = E.T.GP(mu_O, mu_D)
SD1 = SD.T.GP(mu_O, mu_C, sigma1, min, max) 
SD2 = SD.T.GP(mu_O, mu_C, sigma2, min, max) 

y1 = dT(x, mu_O, sigma1, mu_D, min, max, type="GP")
y2 = dT(x, mu_O, sigma2, mu_D, min, max, type="GP")

yC1 = dC(x, mu_O, sigma1, mu_D, min, max, type="GP")
yC2 = dC(x, mu_O, sigma2, mu_D, min, max, type="GP")

yO1 = dO(x, mu_O, sigma1, min, max, type="GP")
yO2 = dO(x, mu_O, sigma2, min, max, type="GP")

EO = mu_O
EC = mu_O + mu_D


xDS = E - mu_D / 2
xDE = E + mu_D / 2

xSDS1 = E - SD1
xSDE1 = E + SD1

xSDS2 = E - SD2
xSDE2 = E + SD2


mY = max(y1,yC1,yO1)
ylim = c(0, mY)
yL1 = 0.1*mY
yL2 = 0.05*mY

# Plot the curve
plot(x, y1, type = "l", lwd = 2, col = "purple", ylim = ylim, ylab="Density", xlab="Response Value (e.g., DOY)")
points(x, yO1, type = "l", col = "red", lwd=0.25)
points(x, yC1, type = "l", col = "blue", lwd=0.25)

# Add filled polygon from x-axis to y
polygon(
  c(x, rev(x)),            # x followed by reverse x
  c(rep(0, length(y1)), rev(y1)),  # 0s (x-axis) then reverse y
  col = rgb(1, 0, 1, alpha = 0.3),  # transparent purple
  border = NA
)
polygon(
  c(x, rev(x)),            # x followed by reverse x
  c(rep(0, length(yO1)), rev(yO1)),  # 0s (x-axis) then reverse y
  col = rgb(1, 0, 0, alpha = 0.3),  # transparent purple
  border = NA
)
polygon(
  c(x, rev(x)),            # x followed by reverse x
  c(rep(0, length(yC1)), rev(yC1)),  # 0s (x-axis) then reverse y
  col = rgb(0, 0, 1, alpha = 0.3),  # transparent purple
  border = NA
)

abline(v=EO,col="red")
abline(v=EC,col="blue")

segments(xDS, yL1, xDE, yL1, col = "black", lwd = 3)
segments(xDS, yL1, xDE, yL1, col = "gray", lwd = 2)
segments(xSDS1, yL2, xSDE1, yL2, col = "black", lwd = 2)

mY = max(y2,yC2,yO2)
ylim = c(0, mY)
yL1 = 0.1*mY
yL2 = 0.05*mY

# Plot the curve
plot(x, y2, type = "l", lwd = 2, col = "purple", ylim = ylim, ylab="Density", xlab="Response Value (e.g., DOY)")
points(x, yO2, type = "l", col = "red", lwd=0.25)
points(x, yC2, type = "l", col = "blue", lwd=0.25)

# Add filled polygon from x-axis to y
polygon(
  c(x, rev(x)),            # x followed by reverse x
  c(rep(0, length(y2)), rev(y2)),  # 0s (x-axis) then reverse y
  col = rgb(1, 0, 1, alpha = 0.3),  # transparent purple
  border = NA
)
polygon(
  c(x, rev(x)),            # x followed by reverse x
  c(rep(0, length(yO2)), rev(yO2)),  # 0s (x-axis) then reverse y
  col = rgb(1, 0, 0, alpha = 0.3),  # transparent purple
  border = NA
)
polygon(
  c(x, rev(x)),            # x followed by reverse x
  c(rep(0, length(yC2)), rev(yC2)),  # 0s (x-axis) then reverse y
  col = rgb(0, 0, 1, alpha = 0.3),  # transparent purple
  border = NA
)

abline(v=EO,col="red")
abline(v=EC,col="blue")

segments(xDS, yL1, xDE, yL1, col = "black", lwd = 3)
segments(xDS, yL1, xDE, yL1, col = "gray", lwd = 2)
segments(xSDS2, yL2, xSDE2, yL2, col = "black", lwd = 2)

