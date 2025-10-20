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

E = phenoCollectR:::E.T.GP(mu_O, mu_D)
SD1 = phenoCollectR:::SD.T.GP(mu_O, mu_C, sigma1, min, max)
SD2 = phenoCollectR:::SD.T.GP(mu_O, mu_C, sigma2, min, max)

y1 = phenoCollectR::dT(x, mu_O, sigma1, mu_D, min, max, type="GP")
y2 = phenoCollectR::dT(x, mu_O, sigma2, mu_D, min, max, type="GP")

yC1 = phenoCollectR:: dC(x, mu_O, sigma1, mu_D, min, max, type="GP")
yC2 = phenoCollectR:: dC(x, mu_O, sigma2, mu_D, min, max, type="GP")

yO1 = phenoCollectR::dO(x, mu_O, sigma1, min, max, type="GP")
yO2 = phenoCollectR::dO(x, mu_O, sigma2, min, max, type="GP")

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
c(x, rev(x)), # x followed by reverse x
c(rep(0, length(y1)), rev(y1)), # 0s (x-axis) then reverse y
col = rgb(1, 0, 1, alpha = 0.3), # transparent purple
border = NA
)
polygon(
c(x, rev(x)), # x followed by reverse x
c(rep(0, length(yO1)), rev(yO1)), # 0s (x-axis) then reverse y
col = rgb(1, 0, 0, alpha = 0.3), # transparent purple
border = NA
)
polygon(
c(x, rev(x)), # x followed by reverse x
c(rep(0, length(yC1)), rev(yC1)), # 0s (x-axis) then reverse y
col = rgb(0, 0, 1, alpha = 0.3), # transparent purple
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
c(x, rev(x)), # x followed by reverse x
c(rep(0, length(y2)), rev(y2)), # 0s (x-axis) then reverse y
col = rgb(1, 0, 1, alpha = 0.3), # transparent purple
border = NA
)
polygon(
c(x, rev(x)), # x followed by reverse x
c(rep(0, length(yO2)), rev(yO2)), # 0s (x-axis) then reverse y
col = rgb(1, 0, 0, alpha = 0.3), # transparent purple
border = NA
)
polygon(
c(x, rev(x)), # x followed by reverse x
c(rep(0, length(yC2)), rev(yC2)), # 0s (x-axis) then reverse y

col = rgb(0, 0, 1, alpha = 0.3), # transparent purple
border = NA
)

abline(v=EO,col="red")
abline(v=EC,col="blue")

segments(xDS, yL1, xDE, yL1, col = "black", lwd = 3)
segments(xDS, yL1, xDE, yL1, col = "gray", lwd = 2)
segments(xSDS2, yL2, xSDE2, yL2, col = "black", lwd = 2)

q1 = phenoCollectR:::qT.GP(0.1, mu_O, sigma1, mu_C = mu_O + mu_D, min, max)
q2 = phenoCollectR:::qT.GP(0.9, mu_O, sigma1, mu_C = mu_O + mu_D, min, max)
print(paste("The difference between the 10% and 90% quantiles for the panel to the left
is ", (q2-q1), "."))

q1 = phenoCollectR:::qT.GP(0.1, mu_O, sigma2, mu_C = mu_O + mu_D, min, max)
q2 = phenoCollectR:::qT.GP(0.9, mu_O, sigma2, mu_C = mu_O + mu_D, min, max)
print(paste("The difference between the 10% and 90% quantiles for the panel to the
right is ", (q2-q1), "."))

#unbiased but weak priors
hp1 = c(180, 20, 50, 20, 10, 10)
data1 = phenoCollectR:::rT.GP(n=1000, mu_O, sigma1, mu_C = mu_O + mu_D, min, max)

stanResult1 = phenoCollectR::runStanPhenology(type="intercept-only", responseData = data1, hyperparams_noCovariates = hp1, partitionDataForPriors = FALSE)

hp2 = c(180, 20, 50, 20, 50, 10)
data2 = phenoCollectR:::rT.GP(n=1000, mu_O, sigma2, mu_C = mu_O + mu_D, min,
max)
stanResult2 = phenoCollectR::runStanPhenology(type="intercept-only", responseData = data2, hyperparams_noCovariates = hp2, partitionDataForPriors = FALSE)

print(paste("The true mean duration (mu_D) is ", mu_D))
print("This result based on Bayesian GP for the left panel is:")
print(stanResult1$sample)
print("This result based on Bayesian GP for the right panel is:")
print(stanResult2$sample)

print(paste("Based on simulated data, the estimate of duration using the 10% and 90% quantiles for the left panel is ", (quantile(data1,0.9) - quantile(data1, 0.1))))
print(paste("Based on simulated data, the estimate of duration using the 10% and 90% quantiles for the right panel is ", (quantile(data2,0.9) - quantile(data2, 0.1))))
