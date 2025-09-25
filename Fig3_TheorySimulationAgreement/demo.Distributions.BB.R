source("phenologyDistributions.BB.R")
source("phenologyInference.BB.R")

mu_O = runif(1,50,300)
mu_D = runif(1,50,90)

sigma_O=runif(1,1,50)
sigma_D=runif(1,1,30)

#PRESET!
mu_O = 150
mu_D = 90
sigma_O = 10
sigma_D = 30

cat(paste("O: ", mu_O, " D: ", mu_D, " sO: ", sigma_O, " sD: ", sigma_D, "\n"))

#some nice asymmetrical distros: [1] "O:  65.836911322549  D:  85.0755408033729  sO:  6.5836911322549  sD:  17.0151081606746"
#another set: "O:  64.360040135216  D:  83.5750018432736  sO:  5.33888339530677  sD:  21.6983206716832"
#another set: "O:  261.89662325196  D:  72.7678886894137  sO:  1.06181577895768  sD:  18.2062792973593"


minResponse=0
maxResponse=365
n = 10000 	#replicates
N = 1000	#popluation size

alpha=0.05

nBin = 100
incC = 0.1

#Monte Carlo simulation
cat("Simulating values\n")
cat("T\n")
T = rT.BB(n=n, mu_O=mu_O, sigma_O=sigma_O, mu_D=mu_D, sigma_D=sigma_D, minResponse=minResponse, maxResponse=maxResponse)
cat("O\n")
O = rO.BB(n=n, mu_O=mu_O, sigma_O=sigma_O, minResponse=minResponse, maxResponse=maxResponse)
cat("C\n")
C = rC.BB(n=n, mu_O=mu_O, sigma_O=sigma_O, mu_D=mu_D, sigma_D=sigma_D, minResponse=minResponse, maxResponse=maxResponse)
cat("Ok1\n")
Ok1 = rOk1.BB(n=n, N=N, mu_O=mu_O, sigma_O=sigma_O, minResponse=minResponse, maxResponse=maxResponse)
cat("CkN\n")
CkN = rCkN.BB(n=n, N=N, mu_O=mu_O, sigma_O=sigma_O, mu_D=mu_D, sigma_D=sigma_D, minResponse=minResponse, maxResponse=maxResponse)

mOk1 = mean(Ok1)
mCkN = mean(CkN)

#Theoretical values
cat("Calculating theoretical values\n")

xlim=range(T, O, C, Ok1, CkN)

inc = (xlim[2]-xlim[1])/((2/3)*nBin)
xlim[1] = xlim[1]-inc
xlim[2] = xlim[2]+inc
if(xlim[1] < minResponse) { xlim[1] = minResponse }
if(xlim[2] > maxResponse) { xlim[2] = maxResponse }
x = seq(xlim[1],xlim[2], inc)
xT = seq(xlim[1],xlim[2], incC)

cat("T\n")
T_theor = dT.BB(xT, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)
cat("O\n")
O_theor = dO.BB(xT, mu_O, sigma_O, minResponse, maxResponse)
cat("C\n")
C_theor = dC.BB(xT, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)
cat("Ok1\n")
Ok1_theor = dOk1.BB(xT, N, mu_O, sigma_O, minResponse, maxResponse)
cat("CkN (This may take a while to compute...)\n")
CkN_theor = dCkN.BB(xT, N, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)

#Plots
cat("Plotting histograms and theoretical values\n")

ylim=range(T_theor,O_theor,C_theor,Ok1_theor,CkN_theor)

layout(matrix(c(1, 1, 2,
		1, 1, 3,
		1, 1, 4), nrow = 3, byrow = TRUE))

# Set minimal margins: c(bottom, left, top, right)
#par(mar = c(2, 4, 1, 1))  # very small top/bottom space

par(mar = c(4, 4, 2, 3))  # Bottom, left, top, right

hist(T, breaks=x, prob=TRUE, col=rgb(1,0,1,0.5),  ylim=ylim, xlab="Day of Year", main=NULL)
hist(O, breaks=x, col=rgb(1,0,0,0.5),  add=TRUE, probability=TRUE)
hist(C, breaks=x, col=rgb(0,0,1,0.5),  add=TRUE, probability=TRUE)
hist(Ok1, breaks=x, col=rgb(1,1,0,0.5),  add=TRUE, probability=TRUE)
hist(CkN, breaks=x, col=rgb(0,1,1,0.5),  add=TRUE, probability=TRUE)

points(xT, T_theor, type='l',col="purple", lwd=3)
points(xT, O_theor, type='l',col="red", lwd=3)
points(xT, C_theor, type='l',col="blue", lwd=3)
points(xT, Ok1_theor, type='l',col="gold4", lwd=3)
points(xT, CkN_theor, type='l',col="cyan4", lwd=3)

cat("Fitting Weibull to extremes\n")
starts <- c(2,2,0)
fitOk1 <- ForestFit::fitWeibull((Ok1-mOk1), location = TRUE, method = "mps", starts = starts)
fitCkN <- ForestFit::fitWeibull(-(CkN-mCkN), location = TRUE, method = "mps", starts = starts)
curve(dweibull(-mOk1 + x - fitOk1$estimate[3], shape = fitOk1$estimate[1], scale = fitOk1$estimate[2]), add = TRUE, col = "black", lwd = 2, lty=2, n = 10000)
curve(dweibull(mCkN-x-fitCkN$estimate[3], shape = fitCkN$estimate[1], scale = fitCkN$estimate[2]), add = TRUE, col = "black", lwd = 2, lty=2, n = 10000)

cat("Peak\n")
peak = getPeak.T.BB(mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)
ind = which.max(T_theor)
points(peak, T_theor[ind], pch=16, cex=2.5, col="purple")
points(peak, T_theor[ind], pch=1, cex=2.5)

cat("Theoretical expected values and probability intervals\n")
EOk1 = E.Ok1.BB(N, mu_O, sigma_O, minResponse, maxResponse)
EO = E.O.BB(mu_O, sigma_O, minResponse, maxResponse)
ET = E.T.BB(mu_O, mu_D, minResponse, maxResponse)
EC = E.C.BB(mu_O, mu_D, minResponse, maxResponse)
ECkN = E.CkN.BB(N, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)
PIOk1 = PI.Ok1.BB(N, mu_O, sigma_O, minResponse, maxResponse, alpha)
PIO = PI.O.BB(mu_O, sigma_O, minResponse, maxResponse, alpha)
PIT = PI.T.BB(mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse, alpha)
PIC = PI.C.BB(mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse, alpha)
PICkN = PI.CkN.BB(N, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse, alpha)

# Get plotting parameters
pin_y <- par("pin")[2]              # Height of plot in inches
usr_y <- par("usr")[4] - par("usr")[3]  # y-axis range in user units
y_units_per_inch <- usr_y / pin_y
lwd <- 10
inch_per_lwd <- 1 / 96
line_thickness_y_units <- lwd * inch_per_lwd * y_units_per_inch
posYInc = line_thickness_y_units / 3

segments(x0 = PIOk1[1], y0 = -5*posYInc, x1 = PIOk1[2], y1 = -5*posYInc, col = "gold4", lwd = 2)
points(x=EOk1, y=-5*posYInc, pch=16, cex=1)
segments(x0 = PIO[1], y0 = -4*posYInc, x1 = PIO[2], y1 = -4*posYInc, col = "red", lwd = 2)
points(x=EO, y=-4*posYInc, pch=16, cex=1)
segments(x0 = PIT[1], y0 = -3*posYInc, x1 = PIT[2], y1 =  -3*posYInc, col = "purple", lwd = 2)
points(x=ET, y=-3*posYInc, pch=16, cex=1)
segments(x0 = PIC[1], y0 = -2*posYInc, x1 = PIC[2], y1 =  -2*posYInc, col = "blue", lwd = 2)
points(x=EC, y=-2*posYInc, pch=16, cex=1)
segments(x0 = PICkN[1], y0 = -5*posYInc, x1 = PICkN[2], y1 = -5*posYInc, col = "cyan4", lwd = 2)
points(x=ECkN, y=-5*posYInc, pch=16, cex=1)


x_center = (xlim[1] + xlim[2]) / 2
y_top = 0.9 * ylim[2]

legend(x = x_center, y = y_top, xjust = 0.5, yjust=1.0,
       legend = c("First Onset","Onset","Observed","Cessation","Last Cessation","Duration", "Range","% in Phenophase","Weibull"),
       col = c("yellow","red","purple","blue","cyan","gray", "brown","green","black"),
       lty = c(1,1,1,1,1,1,1,1,2),
       lwd = 2,
       cex = 1.0, bty="n")

cat("Duration\n")
D = rD.BB(n, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)
xD = seq(min(D),max(D), length.out=nBin/3)
D_theor = dD.BB(xD,mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)
hist(D,breaks=xD, probability=TRUE, xlab="Phenophase Duration (Days)", main=NULL, col="gray")
points(xD, D_theor,type='l', col="gray", lwd=3)

cat("Range\n")
R = rR.BB(n, N, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)
xR = seq(min(R),max(R), length.out=nBin/3)
cat("Range theoretical values may take a while to compute due to the numerical computation of multiple integrals...especially for the large sample size of 10000 used for this figure\n")
R_theor = dR.BB(xR,N,mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)
hist(R,breaks=xR, probability=TRUE, xlab="Phenophase Range (Days)", main=NULL, col="brown")
points(xR, R_theor,type='l', col="brown", lwd=3)

cat("Percent in phenophase\n")
xPNt = seq(0,1,0.001)
t = mean(T)-sd(T)
PNts  = rPNt.BB(n, t, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse, res=1000)
PNt = dPNt.BB(xPNt, t=t, mu_O, sigma_O, mu_D, sigma_D, minResponse, maxResponse)

x = seq(min(PNts),max(PNts),length.out=20)
h = hist(PNts, breaks=x, col=rgb(0,1,0,0.5), probability=TRUE, xlab=paste("Population % in Phenophase (DOY =", round(t), ")"), ylab="Density", main=NULL)
points(xPNt*100, (PNt/max(PNt))*mean(sort(h$density, decreasing = TRUE)[1:3]), type='l', col="darkgreen", lwd=3)

#dev.off()
