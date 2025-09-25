source("phenologyInference.GP.R")
source("phenologyInference.BB.R")
source("phenologyDistributions.GP.R")
source("phenologyDistributions.BB.R")

library(cmdstanr)
library(bayesplot)
library(tidyverse)
library(posterior)
library(dplyr)

#set model parameters
set.seed(42)

min=0
max=365
d = max - min

n = 1000       #replicates
N = 500        #popluation size

#parameter ranges used by simulations
min_mO = 25
max_mO = 340
min_SDO = 5
max_SDO = 21

min_mD = 10
max_mD = 100
min_SDD = 5
max_SDD = 21

#beta scale for hyperparameters
maxPrecision_mO = 5 / d
minPrecision_mO = 10 / d
maxPrecision_SDO = 5 / d
minPrecision_SDO = 10 / d

maxPrecision_mD = 5 / d
minPrecision_mD = 10 / d
maxPrecision_SDD = 5 / d
minPrecision_SDD = 10 / d

maxBias_mO = min_mO / d         #must be less than or equal to min_mO
maxBias_SDO = min_SDO / d
maxBias_mD = min_mD / d
maxBias_SDD = min_SDD / d

#mu_O = runif(1,min_mO, max_mO)
#mu_D = runif(1,min_mD, max_mD)

sigma_O=runif(1,min_SDO, max_SDO)
sigma_D=runif(1,min_SDD, max_SDD)
sigma = sigma_O

#used skewed distributions
mu_O = 150
mu_D = 90
sigma_O = 10
sigma_D = 30

mu_D_scaled = (mu_D/d) * ( 1 - (mu_O - min) / d) * d
mu_C = mu_O + mu_D_scaled

alpha=0.05

nBin = 100
incC = 0.1

#cat(paste("O: ", mu_O, " D: ", mu_D_scaled, " sO: ", sigma_O, " sD: ", sigma_D, "\n"))

#simulate observed values using BB model
#Monte Carlo simulation
cat("Simulating values\n")
cat("T\n")

Ts = rT.BB(n, mu_O, sigma_O, mu_D, sigma_D, min, max)
O = rO.BB(n, mu_O, sigma_O, min, max)
C = rC.BB(n, mu_O, sigma_O, mu_D, sigma_D, min, max)
Ok1 = rOk1.BB(n, N, mu_O, sigma_O, min, max)
cat("Simulating CkN (This may take a while to compute...)\n")
CkN = rCkN.BB(n, N, mu_O, sigma_O, mu_D, sigma_D, min, max)

Ts.GP = rT.GP(n, mu_O, mu_C, sigma, min, max)
O.GP = rO.GP(n, mu_O, sigma)
C.GP = rC.GP(n, mu_C, sigma)
Ok1.GP = rOk1.GP(n, N, mu_O, sigma)
CkN.GP = rCkN.GP(n, N, mu_C, sigma)

#setting plotting limits based on known ranges
xlim=range(Ts, O, C, Ok1, CkN, Ts.GP, O.GP, C.GP, Ok1.GP, CkN.GP)
#xlim.Ok1 = range(Ok1.GP,Ok1)
xlim.Ok1 = range(75,150)
#xlim.O = range(O.GP, O)
xlim.O = range(90,210)
#xlim.T = range(Ts.GP,Ts)
xlim.T = range(100,250)
#xlim.C = range(C.GP,C)
xlim.C = range(140,280)
#xlim.CkN = range(CkN.GP,CkN)
xlim.CkN = range(220,300)
xlim=c(75,300)
inc = (xlim[2]-xlim[1])/((2/3)*nBin)
xlim[1] = xlim[1]-inc
xlim[2] = xlim[2]+inc
if(xlim[1] < min) { xlim[1] = min }
if(xlim[2] > max) { xlim[2] = max }
x = seq(xlim[1],xlim[2], inc)
xT = seq(xlim[1],xlim[2], incC)

#partition the data
n <- length(Ts)
train_indices <- sample(n, size = floor(0.7 * n))

# Create two new data frames
Ts_stan <- Ts[train_indices]       # 70%
Ts_prior <- Ts[-train_indices]      # 30%

#h_m_mO = quantile(Ts_prior, probs = 0.10)
#h_m_mC = quantile(Ts_prior, probs = 0.90)
#h_m_mD = h_m_mC - h_m_mO

#h_sd_mO = 21
#h_sd_mD = 14

#h_m_s = 20
#h_sd_s = 20

h_m_mO = runif(1,(mu_O-min)/d - maxBias_mO, (mu_O-min)/d + maxBias_mO) * d + min
h_sd_mO = runif(1,maxPrecision_mO, minPrecision_mO) * d       #~reciprocal precision, really...

h_m_mD = runif(1,mu_D_scaled/d - maxBias_mD, mu_D_scaled/d + maxBias_mD) * d
h_sd_mD = runif(1,maxPrecision_mD, minPrecision_mD) * d

h_m_s = runif(1,sigma/d - maxBias_SDO, sigma/d + maxBias_SDO) * d
h_sd_s = runif(1,maxPrecision_SDO, minPrecision_SDO) * d

h_m_sD = runif(1,sigma_D/d - maxBias_SDO, sigma_D/d + maxBias_SDO) * d
h_sd_sD = runif(1,maxPrecision_SDO, minPrecision_SDO) * d

hyperparameters = c(h_m_mO, h_sd_mO, h_m_mD, h_sd_mD, h_m_s, h_sd_s)

#infer parameters using stan
res = runStan.NoCovariates.T.GP(fileOrData=Ts_stan, minResponse=min, maxResponse=max, hyperparameters = hyperparameters, dataProvided=TRUE, runMAP=FALSE, setStringent=TRUE, processExtremes=TRUE, N=N)

#extract parameter mean estimates
summary = res$sample$summary(variables=c("mu_O","mu_D","mu_C","sigma"),posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()

#mu_O = 150
#mu_D = 90
#sigma_O = 10
#sigma_D = 30
#hyperparameters = c(h_m_mO,h_sd_mO,h_m_mD,h_sd_mD,h_m_s,h_sd_s,h_m_sD,h_sd_sD)
hyperparameters = c(150,5,90,5,10,2,30,2)

cat("Running MAP estimation under BB model. This may take a while.\n")

MAP = getMAP.T.BB(fileOrData=Ts_stan, min=min, max=max,minS=1, maxS=3000,  init_params = c(150,10,90,30), hyperparameters = hyperparameters, dataProvided=TRUE)

#MAP = getMAP.T.BB(fileOrData=Ts_stan, min=min, max=max,minS=1, maxS=3000,  hyperparameters = hyperparameters, dataProvided=TRUE)
cat("Done running MAP estimation under BB model.\n")

mu_O.GP = summary$mean[summary$variable=="mu_O"]
mu_D.GP = summary$mean[summary$variable=="mu_D"]
mu_C.GP = summary$mean[summary$variable=="mu_C"]
sigma.GP = summary$mean[summary$variable=="sigma"]

cat("\n\nTrue parameters\n")
cat(paste("O: ", mu_O, " D: ", mu_D_scaled, " sO: ", sigma_O, " sD: ", sigma_D, "\n\n"))

#cat("hyperparameters\n")
#print(hyperparameters)

cat("\n\nsummary\n")
print(summary)
cat("\n\n\n")

#calculating theoretical PDFs
T_theor.BB = dT.BB(xT, mu_O, sigma_O, mu_D, sigma_D, min, max)
O_theor.BB = dO.BB(xT, mu_O, sigma_O, min, max)
C_theor.BB = dC.BB(xT, mu_O, sigma_O, mu_D, sigma_D, min, max)
Ok1_theor.BB = dOk1.BB(xT, N, mu_O, sigma_O, min, max)
cat("Calculating theoretical CkN (This may take a while to compute...)\n")
CkN_theor.BB = dCkN.BB(xT, N, mu_O, sigma_O, mu_D, sigma_D, min, max)

EOk1.BB = E.Ok1.BB(N, mu_O, sigma_O, min, max)
EO.BB = E.O.BB(mu_O, sigma_O, min, max)
ET.BB = E.T.BB(mu_O, mu_D, min, max)
EC.BB = E.C.BB(mu_O, mu_D, min, max)
ECkN.BB = E.CkN.BB(N, mu_O, sigma_O, mu_D, sigma_D, min, max)
#PIOk1.BB = PI.Ok1.BB(N, mu_O, sigma_O, min, max, alpha)
#PIO.BB = PI.O.BB(mu_O, sigma_O, min, max, alpha)
#PIT.BB = PI.T.BB(mu_O, sigma_O, mu_D, sigma_D, min, max, alpha)
#PIC.BB = PI.C.BB(mu_O, sigma_O, mu_D, sigma_D, min, max, alpha)
#PICkN.BB = PI.CkN.BB(N, mu_O, sigma_O, mu_D, sigma_D, min, max, alpha)

#T_theor.BB = dT.GP(xT, mu_O, mu_C, sigma, min, max)
#O_theor.BB = dO.GP(xT, mu_O, sigma)
#C_theor.BB = dC.GP(xT, mu_C, sigma)
#Ok1_theor.BB = dOk1.GP(xT, N, mu_O, sigma)
#CkN_theor.BB = dCkN.GP(xT, N, mu_C, sigma)

T_theor.GP = dT.GP(xT, mu_O.GP, mu_C.GP, sigma.GP, min, max)
O_theor.GP = dO.GP(xT, mu_O.GP, sigma.GP)
C_theor.GP = dC.GP(xT, mu_C.GP, sigma.GP)
Ok1_theor.GP = dOk1.GP(xT, N, mu_O.GP, sigma.GP)
CkN_theor.GP = dCkN.GP(xT, N, mu_C.GP, sigma.GP)

EOk1.GP = E.Ok1.GP(N, mu_O.GP, sigma.GP, min, max)
EO.GP = E.O.GP(mu_O.GP)
ET.GP = E.T.GP(mu_O.GP, mu_D.GP)
EC.GP = E.C.GP(mu_C.GP)
ECkN.GP = E.CkN.GP(N, mu_C.GP, sigma.GP, min, max)

PIOk1.GP = PI.Ok1.GP(N, mu_O.GP, sigma.GP, alpha)
PIO.GP = PI.O.GP(mu_O.GP, sigma.GP, alpha)
PIT.GP = PI.T.GP(mu_O.GP, mu_C.GP, sigma.GP, alpha)
PIC.GP = PI.C.GP(mu_C.GP, sigma.GP, alpha)
PICkN.GP = PI.CkN.GP(N, mu_C.GP, sigma.GP, alpha)

#Replace with 95%CI...
PIO.GP[1] = summary$q2.5[summary$variable=="mu_O"]
PIO.GP[2] = summary$q97.5[summary$variable=="mu_O"]
PIC.GP[1] = summary$q2.5[summary$variable=="mu_C"]
PIC.GP[2] = summary$q97.5[summary$variable=="mu_C"]

PIT.GP[1] = (PIC.GP[1] + PIO.GP[1]) / 2
PIT.GP[2] = (PIC.GP[2] + PIO.GP[2]) / 2

PIOk1.GP = quantile(res$draws_extremes$E_Ok1, probs = c(0.025, 0.975))
PICkN.GP = quantile(res$draws_extremes$E_CkN, probs = c(0.025, 0.975))

#ylim.Ok1 = range(Ok1_theor.GP,Ok1_theor.BB)
ylim.Ok1 = range(0,0.16)
#ylim.O = range(O_theor.GP, O_theor.BB)
ylim.O = range(0,0.05)
#ylim.T = range(T_theor.GP,T_theor.BB)
ylim.T = range(0,0.025)
#ylim.C = range(C_theor.GP,C_theor.BB)
ylim.C = range(0,0.05)
#ylim.CkN = range(CkN_theor.GP,CkN_theor.BB)
ylim.CkN = range(0,0.09)

ylim=range(T_theor.GP,O_theor.GP,C_theor.GP,Ok1_theor.GP,CkN_theor.GP, T_theor.BB,O_theor.BB,C_theor.BB,Ok1_theor.BB,CkN_theor.BB)
#ylim=range(T_theor.GP,O_theor.GP,C_theor.GP, T_theor.BB,O_theor.BB,C_theor.BB)

cat("Plotting peak point.\n")
peak = getPeak.T.BB(mu_O, sigma_O, mu_D, sigma_D, min, max)
ind = which.max(T_theor.BB)
cat("Plotting MAP peak point\n.")
#Draw the MAP inferrence under beta-beta
MAPp = MAP$par_orig_scale
print(MAP$par_orig_scale)

MAP_T = dT.BB(x=xT, mu_O = MAPp[1], sigma_O=MAPp[2], mu_D=MAPp[3], sigma_D=MAPp[4], min=min, max=max)
peakMAP = getPeak.T.BB(mu_O = MAPp[1], sigma_O=MAPp[2], mu_D=MAPp[3], sigma_D=MAPp[4], min=min, max=max)
ind = which.max(MAP_T)
cat(paste0("Peak at ", peak, " position ", T_theor.BB[ind], "\n"))
#points(peak, T_theor.BB[ind], pch=16, cex=1.25, col="purple")
#points(peakMAP, MAP_T[ind], pch=1, cex=1.25, lwd=0.5)



#set up matrix of plots
layout_matrix <- matrix(c(
  1, 1,   # row 1: plot 1 spans both columns
  2, 3,   # row 2: plots 2 and 3
  4, 5    # row 3: plots 4 and 5
), nrow = 3, byrow = TRUE)

# apply layout
layout(layout_matrix)

# optional: add some spacing
par(mar = c(2,2,2,1))
#par(mar = c(4, 4, 2, 3))  # Bottom, left, top, right
# Plot expected values and ranges
pin_y <- par("pin")[2]              # Height of plot in inches
usr_y <- par("usr")[4] - par("usr")[3]  # y-axis range in user units
y_units_per_inch <- usr_y / pin_y
lwd <- 10
inch_per_lwd <- 1 / 96 
line_thickness_y_units <- lwd * inch_per_lwd * y_units_per_inch
posYInc = line_thickness_y_units / 3

#Plot 1: Observed data graph
#_________
#draw histogram of simulated data
hist(Ts, breaks=x, prob=TRUE, col=rgb(1,0,1,0.5),  ylim=ylim.T, xlim=xlim.T, xlab="Day of Year", main=NULL)
#draw solid lines of inferred distributions
points(xT, T_theor.GP, type='l',col="purple", lwd=3)
#Draw the MAP inferrence under beta-beta
points(xT, MAP_T, type='l',lwd=0.5)
#draw polygons of true distributions
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(T_theor.BB)),  
        col = rgb(1,0,1, 0.4), border = NA)
cat("Plotting vertical lines.\n")
#abline(v = ET.BB, col="purple")
abline(v = ET.BB, col="black")
cat("Plotting line segments point.\n")
segments(x0 = PIT.GP[1], y0 = 0, x1 = PIT.GP[2], y1 =  0, col = "purple", lwd = 5)
points(x=ET.GP, y=0, pch=16, cex=1.5)
#put the peaks at the end so there is nothing obscurring them
points(peak, 0, pch=16, cex=2, col="purple")
points(peakMAP, 0, pch=16, cex=2, lwd=0.5)
points(peakMAP, 0, pch=16, cex=0.5, lwd=0.5, col="yellow")
points(peak, 0, pch=16, cex=0.5, col="yellow")
#_________

# Plot expected values and ranges
#Plot 2: First onset
#draw solid lines of inferred distributions
plot(xT, Ok1_theor.GP, type='l',col="gold4", lwd=3, ylim=ylim.Ok1, xlim=xlim.Ok1)
#Draw the MAP inferrence under beta-beta
points(xT, dOk1.BB(x=xT,N=N, mu_O = MAPp[1], sigma_O=MAPp[2], min=min,max=max ), type='l',lwd=0.5)
#draw polygons of true distributions
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(Ok1_theor.BB)),  
        col = rgb(1,1,0, 0.4), border = NA)
cat("Plotting vertical lines.\n")
#abline(v = EOk1.BB, col="yellow")
abline(v = EOk1.BB, col="black")
cat("Plotting line segments point.\n")
segments(x0 = PIOk1.GP[1], y0 = 0, x1 = PIOk1.GP[2], y1 = 0, col = "gold4", lwd = 5)
points(x=EOk1.GP, y=0, pch=16, cex=1.5)

#_________
# Plot expected values and ranges
#Plot 3: onset
#draw solid lines of inferred distributions
plot(xT, O_theor.GP, type='l',col="red", lwd=3, xlim=xlim.O, ylim=ylim.O)
#Draw the MAP inferrence under beta-beta
points(xT, dO.BB(x=xT,mu_O = MAPp[1], sigma_O=MAPp[2], min=min,max=max ), type='l',lwd=0.5)
#draw polygons of true distributions
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(O_theor.BB)),  
        col = rgb(1,0,0, 0.4), border = NA)
cat("Plotting vertical lines.\n")
#abline(v = EO.BB, col="red")
abline(v = EO.BB, col="black")
cat("Plotting line segments point.\n")
segments(x0 = PIO.GP[1], y0 = 0, x1 = PIO.GP[2], y1 =  0, col = "red", lwd = 5)
points(x=EO.GP, y=0, pch=16, cex=1.5)

#_________
# Plot expected values and ranges
#Plot 4: cessation
#draw solid lines of inferred distributions
plot(xT, C_theor.GP, type='l',col="blue", lwd=3, ylim=ylim.C, xlim=xlim.C)
#Draw the MAP inferrence under beta-beta
points(xT, dC.BB(x=xT, mu_O = MAPp[1], sigma_O=MAPp[2], mu_D=MAPp[3], sigma_D=MAPp[4], min=min, max=max), type='l',lwd=0.5)
#draw polygons of true distributions
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(C_theor.BB)),  
        col = rgb(0,0,1, 0.4), border = NA)
cat("Plotting vertical lines.\n")
#abline(v = EC.BB, col="blue")
abline(v = EC.BB, col="black")
cat("Plotting line segments point.\n")
segments(x0 = PIC.GP[1], y0 = 0, x1 = PIC.GP[2], y1 = 0, col = "blue", lwd = 5)
points(x=EC.GP, y=0, pch=16, cex=1.5)

#_________
# Plot expected values and ranges
#Plot 5: last cessation
#draw solid lines of inferred distributions
plot(xT, CkN_theor.GP, type='l',col="cyan4", lwd=3, xlim=xlim.CkN, ylim=ylim.CkN)
#Draw the MAP inferrence under beta-beta
points(xT, dCkN.BB(x=xT, N=N, mu_O = MAPp[1], sigma_O=MAPp[2], mu_D=MAPp[3], sigma_D=MAPp[4], min=min, max=max), type='l',lwd=0.5)
#draw polygons of true distributions
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(CkN_theor.BB)),  
        col = rgb(0,1,1, 0.4), border = NA)
cat("Plotting vertical lines.\n")
#abline(v = ECkN.BB, col="cyan")
abline(v = ECkN.BB, col="black")
cat("Plotting line segments point.\n")
segments(x0 = PICkN.GP[1], y0 = 0, x1 = PICkN.GP[2], y1 = 0, col = "cyan4", lwd = 5)
points(x=ECkN.GP, y=0, pch=16, cex=1.5)

#_________





