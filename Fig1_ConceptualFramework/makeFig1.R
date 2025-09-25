rm(list = ls())

#source("../SharedCode/theoryOfPhenology.txt.R")
#source("../SharedCode/simulatePhenology_edit.txt.R")
#source("../SharedCode/inferPhenology_BayesianMCMC.txt.R")

source("theoryOfPhenology.txt.R")
source("simulatePhenology_edit.txt.R")
source("inferPhenology_BayesianMCMC.txt.R")

#vi theoryOfPhenology.txt.R
#vi simulatePhenology_edit.txt.R
#vi inferPhenology_BayesianMCMC.txt.R

#somewhat arbitrary parameters with mean onset around 110, mean duration around 20 days
oParList=list(shape1=90.27611,shape2=244.8296)	
dParList=list(shape1=42.84,shape2=233.6238)		
onset = "beta"
duration = "beta"
min = 0
max = 365
N = 10000
subSS = 5000
subSubSS = 1000
show=25

pdf("Figure1_ConceptualFraming.pdf")


data = simulatePhenology(
			 N=N,
			 subSS = subSS, 
			 min = min,
			 max = max,
			 onset = onset,
			 duration = duration,
			 oParList = oParList,
			 dParList = dParList
			 )

attach(data)

observations1 = sample(observations,subSubSS,replace=F)

xl = min(onsets-5)
xh = max(onsets+durations+5)

xlim=c(xl,xh)
par(mfrow=c(4,1),mai = c(0.8, 1, 0, 0.1))

#make graph of individuals showing onset (red), durations (black), cessation (blue), and sampled days (purple)

plot(1, type = "n", xlim=xlim, ylim=c(0,show), xlab="Day of Year", ylab="Individual", main="", bty="n")
inds = sample(1:N,show,replace=F)
y = seq(1,show,by=1)
points( onsets[inds], y, col="red", pch=20, cex = 1)
points( onsets[inds]+durations[inds], y, col="blue", pch=20, cex = 1)
segments( onsets[inds], y, onsets[inds]+durations[inds], y, col="black")


ys = double()
xs = double()
cnt = 1
for(i in 1:N)
{
	#inefficient, but sufficient!
	if(!is.na((pos = match(observations.inds[i],inds)))) 
	{
		ys[cnt] = pos
		xs[cnt] = observations[i]
		cnt = cnt+1
	}
}
points(xs,ys,col="purple", pch=4, cex =1)



#make histograms of onsets and cessations in the simulated population
hist(onsets,col=rgb(1,0,0,0.1),xlim=xlim, breaks=seq(xl,xh,2),main=NULL,xlab="Day of Year", ylab="Frequency")
abline(v=mean(onsets),col="black")
hist(onsets+durations,col=rgb(0,0,1,0.1),add=T, breaks=seq(xl,xh,2))
abline(v=mean(onsets+durations),col="black")
abline(v=true.minTime, col="black")
abline(v=true.maxTime, col="black")

#make histogram of observations of times in the simulated population
hist(observations1,col=rgb(1,0,1,0.1), xlim=xlim, breaks=seq(xl,xh,2), xlab="Day of Year", ylab="Frequency", main=NULL)
abline(v=mean(observations1),col="black")


#make curves of inferred distributions
m=min
M=max
n=2000

#	infer parameters using ML (for the sake of the figure, this step is skipped in favor of using the true values)
#print("Inferring ML parameter estimates")
#print(oParList)
#print(dParList)
#parList.est = getShapeParams.ML.estimate(observations1, min=m, max=M, oParList=oParList, dParList=dParList)
#oParList1 = parList.est$oParList
#dParList1 = parList.est$dParList 
#print("Done inferring ML parameter estimates")
#print(parList.est)
#print(oParList1)
#print(dParList1)

oParList1 = oParList
dParList1 = dParList

fCs = f_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList=oParList1, dParList=dParList1)
f_C = f_C.function(x=fCs$x, y=fCs$y)
CDF = F_C.vector(x=fCs$x, y=fCs$y)
F_C = F_C.function(x=fCs$x, y=CDF)
Pts = P_t.vector(m=m, M=M, n=n, onset=onset, oParList=oParList1, F_C.func = F_C)
fXs = f_X.vector(x=fCs$x, Pts)

#fCs_r = f_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList=oParList, dParList=dParList)
#f_C_r = f_C.function(x=fCs_r$x, y=fCs_r$y)
#CDF_r = F_C.vector(x=fCs_r$x, y=fCs_r$y)
#F_C_r = F_C.function(x=fCs_r$x, y=CDF)
#Pts_r = P_t.vector(m=m, M=M, n=n, onset=onset, oParList=oParList, F_C.func = F_C_r)
#fXs_r = f_X.vector(x=fCs_r$x, Pts_r)

##f_X = f_X.function(x=fCs$x, y=fXs)
##FXs = F_X.vector( x=fCs$x, y=fXs) 
##F_X = F_X.function(x=fCs$x, y=FXs)
##P_t =   P_t.function(x=fCs$x, y=Pts)
##Nts = f_N_t(t=t, N=N, P_t.func = P_t)


lylim = 0
uylim = 0.05
# hist of data, probability is true
#hist(observations1, probability=T, xlim=xlim, ylim=c(lylim,uylim), breaks=seq(xl,xh,2), border=rgb(0,0,0,1), col=rgb(1,0,1,0.5), main=NULL, xlab = "Day of year")
plot(1, type = "n", bty="n", xlim=xlim, ylim=c(lylim,uylim), xlab="Day of Year", ylab="Density", main="")

#f_x
points(fCs$x, fXs, type="l",col="purple")
abline(v=sum(fCs$x*fXs/sum(fXs)),col="purple")
polygon(fCs$x, fXs, col=rgb(1,0,1,0.1), border=NA)

#points(fCs_r$x, fXs_r, type="l",col="yellow", lty=2)
#abline(v=sum(fCs_r$x*fXs_r/sum(fXs)),col="yellow", lty=2)

#f_o_scaled
xs = seq(0,1,0.001)
ys = dbeta(xs, shape1=oParList1[[1]], shape2=oParList1[[2]]) / (M - m)
#ys_r = dbeta(xs, shape1=oParList[[1]], shape2=oParList[[2]]) / (M - m)
xs = m + (M-m)*xs
points(xs, ys , type="l", col=rgb(1,0,0,1))
abline(v=sum(xs*ys/sum(ys)),col="black")
polygon(xs, ys, col=rgb(1,0,0,0.1) , border=NA)

#points(xs, ys_r , type="l", col="yellow", lty=2)
#abline(v=sum(xs*ys_r/sum(ys_r)),col="yellow", lty=2)

#f_c_scaled
points(fCs$x, fCs$y, type="l", col=rgb(0,0,1,1))
abline(v=sum(fCs$x*fCs$y/sum(fCs$y)),col="black")
polygon(fCs$x, fCs$y, col=rgb(0,0,1,0.1) , border=NA)

#points(fCs_r$x, fCs_r$y, type="l", col="yellow", lty=2)
#abline(v=sum(fCs_r$x*fCs_r$y/sum(fCs_r$y)),col="yellow", lty=2)

#print("recap")
#print(oParList1)
#print(oParList1[[1]])
#print(oParList1[[2]])
#print(dParList1)

#fxk=1 
k=1
fXk1_N = N*dbeta((fCs$x-m)/(M-m),oParList1[[1]],oParList1[[2]])*choose(N-1,k-1)*(pbeta((fCs$x-m)/(M-m),oParList1[[1]],oParList1[[2]])^(k-1))*((1-pbeta((fCs$x-m)/(M-m),oParList1[[1]],oParList1[[2]]))^(N-k))
abline(v=sum(fCs$x*fXk1_N/sum(fXk1_N)), col="black")
fXk1_N = lylim + (uylim-lylim)*fXk1_N/max(fXk1_N)
points(fCs$x, fXk1_N, type="l", col=rgb(1,204/255,0,1))
polygon(fCs$x, fXk1_N, col=rgb(1,204/255,0,0.1) , border=NA)

#fXk1_N_r = N*dbeta((fCs_r$x-m)/(M-m),oParList[[1]],oParList[[2]])*choose(N-1,k-1)*(pbeta((fCs_r$x-m)/(M-m),oParList[[1]],oParList[[2]])^(k-1))*((1-pbeta((fCs_r$x-m)/(M-m),oParList[[1]],oParList[[2]]))^(N-k))
#abline(v=sum(fCs_r$x*fXk1_N_r/sum(fXk1_N_r)), col="yellow", lty=2)
#fXk1_N_r = lylim + (uylim-lylim)*fXk1_N_r/max(fXk1_N_r)
#points(fCs_r$x, fXk1_N_r, type="l", col="yellow", lty=2)
##polygon(fCs_r$x, fXk1_N_r, col=rgb(1,0,0,0.5) , border=NA)


#print("recap")
#print(oParList1)
#print(dParList1)

#fxk=N
k=N
fXkN_N = f_X_k(fCs$x, N=N, k=k, f_X.func=f_C, F_X.func=F_C)
abline(v=sum(fCs$x*fXkN_N/sum(fXkN_N)), col="black")
fXkN_N = lylim + (uylim-lylim)*fXkN_N/max(fXkN_N)
points(fCs$x, fXkN_N, type="l", col=rgb(0,1,1,1))
polygon(fCs$x, fXkN_N, col=rgb(0,1,1,0.1) , border=NA)

#fXkN_N_r = f_X_k(fCs_r$x, N=N, k=k, f_X.func=f_C_r, F_X.func=F_C_r)
#abline(v=sum(fCs_r$x*fXkN_N_r/sum(fXkN_N_r)), col="yellow", lty=2)
#fXkN_N_r = lylim + (uylim-lylim)*fXkN_N_r/max(fXkN_N_r)
#points(fCs_r$x, fXkN_N_r, type="l", col="yellow", lty=2)
##polygon(fCs_r$x, fXkN_N_r, col=rgb(0,0,1,0.5) , border=NA)

#segments(250,0,365,0,col="white",lwd=3)
#segments(0,0,250,0,col="black",lwd=1)


#legend(265,0.0425, legend=c("PDF X1", "PDF O", "PDF X", "observed data", "PDF C", "PDF XN"),
       #col=c("black","red","purple","purple","blue","black"), lwd=1.5)

dev.off()




