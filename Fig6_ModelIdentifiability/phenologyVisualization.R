source("phenologyDistributions.R")

visualizeSimulatedData_GP = function(sim, nbreaks=100, col=c(rgb(1,0,0,0.5),rgb(1,0,1,0.5),rgb(0,0,1,0.5)),min=0, max=365) {
	xmin = ifelse(min(sim$t_start) < min - 0.1*(max-min), min(sim$t_start), min - 0.2*(max-min))
	xmax = ifelse(max(sim$t_end) > max + 0.1*(max-min), max(sim$t_end), max + 0.2*(max-min))
	curve(dfirst_start_time_GP(x,N=sim$n, mu_O=sim$onset_mean, sigma=sim$sigma), xlim=c(xmin, xmax),n=1000)
	curve(dstart_time_GP(x,mu_O=sim$onset_mean, sigma=sim$sigma), add=T,n=1000)
	curve(dobserved_GP(x,mean_s=sim$onset_mean, mean_e=sim$cessation_mean, sigma=sim$sigma), add=T,n=1000)
	curve(dend_time_GP(x,mu_C=sim$cessation_mean, sigma=sim$sigma), add=T,n=1000)
	curve(dlast_end_time_GP(x,N=sim$n,mu_C=sim$cessation_mean, sigma=sim$sigma), add=T,n=1000)
	abline(v=c(sim$onset_mean, sim$observed_mean, sim$cessation_mean, sim$e_last_end_time[1], sim$e_first_start_time[1]))

	hist(sim$t_start, breaks=seq(min,max,length.out=nbreaks), col=col[1], prob=T, add=T)
	hist(sim$observed, breaks=seq(min,max,length.out=nbreaks), col=col[2], add=T, prob=T)
	hist(sim$t_end, breaks=seq(min,max,length.out=nbreaks), col=col[3], add=T, prob=T)
}

do_PosteriorComparisonToSimulated_GP = function(posteriorResults,sim,nbreaks=25, min=0, max=365) {
	par = posteriorResults$par

	#print("hists")

	sim$t_start = (sim$t_start - min) / (max - min)
	sim$t_end = (sim$t_end - min) / (max - min)
	sim$observed = (sim$observed - min) / (max - min)
	sim$onset_mean = (sim$onset_mean - min) / (max - min)
	sim$cessation_mean = (sim$cessation_mean - min) / (max - min)
	sim$sigma = (sim$sigma) / (max - min)


hist(sim$t_start, breaks = seq(0,1,length.out=nbreaks), xlim=c(0,1), col = rgb(1,0,0,0.2), prob=T )
hist(sim$observed, breaks = seq(0,1,length.out=nbreaks), col = rgb(1,0,1,0.5), prob=T, add=T)
hist(sim$t_end, breaks = seq(0,1,length.out=nbreaks), col = rgb(0,0,1,0.2), prob=T, add=T)

res = 1000
	#print("observed")
curve(dobserved(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dobserved_GP(x, sim$onset_mean, sim$cessation_mean, sim$sigma),
      add = TRUE, col = "purple", lwd = 1, n=res)

#onset
	#print("onset")
curve(dstart_time(x, par[1], par[2]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dstart_time_GP(x, sim$onset_mean, sim$sigma),
      add = TRUE, col = "red", lwd = 1, n=res)

#cessation
	#print("cessation")
curve(dend_time(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dend_time_GP(x, sim$cessation_mean, sim$sigma),
      add = TRUE, col = "blue", lwd = 1, n=res)
	abline(v=c(sim$onset_mean, (sim$observed_mean-1)/(max-min), sim$cessation_mean), lwd=2)

	#print("stats")
MAP_mean_s = beta_mean(par[1], par[2])
MAP_sd_s = beta_sd(par[1], par[2])*(max-min)
MAP_mean_d = beta_mean(par[3], par[4]) * (1 - MAP_mean_s) * (max-min)
MAP_mean_s = min + MAP_mean_s * (max - min)
MAP_sd_d = beta_sd(par[3], par[3])*(max-min)
	abline(v=c((MAP_mean_s-min)/(max-min), expected_observed(par[1],par[2],par[3],par[4]), (MAP_mean_s + MAP_mean_d - min)/(max-min)), col=c("red","purple","blue"))

#This DOESN'T TAKE INTO ACCOUNT THE CONTRACTION in durations due to the non-independence with onset
onset_mean = min + sim$onset_mean * (max - min)
onset_sd = sim$sigma * (max - min)
duration_mean = sim$duration_mean
duration_sd = 0 

comparison_table <- data.frame(
  Parameter = c("Start time mean", "Start time sd", "Duration mean", "Duration sd"),
  True_Value = c(onset_mean, onset_sd, duration_mean, duration_sd),
  Estimated_Value = c(MAP_mean_s, MAP_sd_s, MAP_mean_d, MAP_sd_d)
)

print(comparison_table)
}

# --- 9. Overlay fitted density on histogram ---
do_PosteriorComparisonToSimulated = function(posteriorResults,sim,nbreaks=25, min=0, max=365) {
	par = posteriorResults$par

hist(sim$t_start, breaks = seq(0,1,length.out=nbreaks), xlim=c(0,1), col = rgb(1,0,0,0.2), prob=T )
hist(sim$observed, breaks = seq(0,1,length.out=nbreaks), col = rgb(1,0,1,0.5), prob=T, add=T)
hist(sim$t_end, breaks = seq(0,1,length.out=nbreaks), col = rgb(0,0,1,0.2), prob=T, add=T)

res = 1000
curve(dobserved(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dobserved(x, sim$alpha_s, sim$beta_s,
                        sim$alpha_d, sim$beta_d),
      add = TRUE, col = "purple", lwd = 2, n=res)

#onset
curve(dstart_time(x, par[1], par[2]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dstart_time(x, sim$alpha_s, sim$beta_s),
      add = TRUE, col = "red", lwd = 2, n=res)

#cessation
curve(dend_time(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dend_time(x, sim$alpha_s, sim$beta_s, sim$alpha_d, sim$beta_d),
      add = TRUE, col = "blue", lwd = 2, n=res)

MAP_mean_s = min + beta_mean(par[1], par[2])*(max-min)
MAP_sd_s = beta_sd(par[1], par[2])*(max-min)
MAP_mean_d = beta_mean(par[3], par[4])*(max-min)
MAP_sd_d = beta_sd(par[3], par[3])*(max-min)

#This DOESN'T TAKE INTO ACCOUNT THE CONTRACTION in durations due to the non-independence with onset
onset_mean = min + beta_mean(sim$alpha_s, sim$beta_s)*(max-min)
onset_sd = beta_sd(sim$alpha_s, sim$beta_s)*(max-min)
duration_mean = beta_mean(sim$alpha_d, sim$beta_d) *(max-min)
duration_sd = beta_sd(sim$alpha_d, sim$beta_d) *(max-min)

comparison_table <- data.frame(
  Parameter = c("Start time mean", "Start time sd", "Duration mean", "Duration sd"),
  True_Value = c(onset_mean, onset_sd, duration_mean, duration_sd),
  Estimated_Value = c(MAP_mean_s, MAP_sd_s, MAP_mean_d, MAP_sd_d)
)

print(comparison_table)
}

doFitDistComparisonToSimulated = function(fit,sim,nbreaks=25) {
hist(sim$t_start/365, breaks = seq(0,1,length.out=nbreaks), xlim=c(0,1), col = rgb(1,0,0,0.2), prob=T )
hist(sim$observed/365, breaks = seq(0,1,length.out=nbreaks), col = "purple", prob=T, main = "Fit of observed", xlab = "Time", add=T)
hist(sim$t_end/365, breaks = seq(0,1,length.out=nbreaks), col = rgb(0,0,1,0.2), prob=T, add=T)

res = 1000
curve(dobserved(x, fit$estimate["alpha_s"], fit$estimate["beta_s"],
                        fit$estimate["alpha_d"], fit$estimate["beta_d"]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dobserved(x, sim$alpha_s, sim$beta_s,
                        sim$alpha_d, sim$beta_d),
      add = TRUE, col = "purple", lwd = 2, n=res)

#onset
curve(dstart_time(x, fit$estimate["alpha_s"], fit$estimate["beta_s"]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dstart_time(x, sim$alpha_s, sim$beta_s),
      add = TRUE, col = "red", lwd = 2, n=res)

#cessation
curve(dend_time(x, fit$estimate["alpha_s"], fit$estimate["beta_s"],
                        fit$estimate["alpha_d"], fit$estimate["beta_d"]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dend_time(x, sim$alpha_s, sim$beta_s, sim$alpha_d, sim$beta_d),
      add = TRUE, col = "blue", lwd = 2, n=res)

MLE_mean_s = beta_mean(fit$estimate["alpha_s"], fit$estimate["beta_s"])
MLE_sd_s = beta_sd(fit$estimate["alpha_s"], fit$estimate["beta_s"])
MLE_mean_d = beta_mean(fit$estimate["alpha_d"], fit$estimate["beta_d"])
MLE_sd_d = beta_sd(fit$estimate["alpha_d"], fit$estimate["beta_d"])

MLE_mean_s_SE = compute_se(fit$estimate["alpha_s"], fit$estimate["beta_s"], fit$sd["alpha_s"], fit$sd["beta_s"])$se_mean
MLE_sd_s_SE = compute_se(fit$estimate["alpha_s"], fit$estimate["beta_s"], fit$sd["alpha_s"], fit$sd["beta_s"])$se_sd
MLE_mean_d_SE = compute_se(fit$estimate["alpha_d"], fit$estimate["beta_d"], fit$sd["alpha_d"], fit$sd["beta_d"])$se_mean
MLE_sd_d_SE = compute_se(fit$estimate["alpha_d"], fit$estimate["beta_d"], fit$sd["alpha_d"], fit$sd["beta_d"])$se_sd

#This DOESN'T TAKE INTO ACCOUNT THE CONTRACTION in durations due to the non-independence with onset
onset_mean = beta_mean(sim$alpha_s, sim$beta_s)
onset_sd = beta_sd(sim$alpha_s, sim$beta_s)
duration_mean = beta_mean(sim$alpha_d, sim$beta_d) 
duration_sd = beta_sd(sim$alpha_d, sim$beta_d) 

comparison_table <- data.frame(
  Parameter = c("Start time mean", "Start time sd", "Duration mean", "Duration sd"),
  True_Value = c(onset_mean, onset_sd, duration_mean, duration_sd),
  Estimated_Value = c(MLE_mean_s, MLE_sd_s, MLE_mean_d, MLE_sd_d),
  Estimated_Value_SE = c(MLE_mean_s_SE, MLE_sd_s_SE, MLE_mean_d_SE, MLE_sd_d_SE),
  Parameter_q2.5 = c(MLE_mean_s - 1.96*MLE_mean_s_SE, MLE_sd_s - 1.96*MLE_sd_s_SE, MLE_mean_d - 1.96*MLE_mean_d_SE, MLE_sd_d - 1.96*MLE_sd_d_SE),
  Parameter_q97.5 = c(MLE_mean_s + 1.96*MLE_mean_s_SE, MLE_sd_s + 1.96*MLE_sd_s_SE, MLE_mean_d + 1.96*MLE_mean_d_SE, MLE_sd_d + 1.96*MLE_sd_d_SE)
)

print(comparison_table)
}


do_PosteriorComparisonToEmpirical = function(posteriorResults,observed,nbreaks=25,min=0,max=365) {
	par = posteriorResults$par

hist(observed, breaks = seq(0,1,length.out=nbreaks), xlim=c(0,1), ylim=c(0,20), col = rgb(1,0,0,0.2), prob=T )

res = 1000
curve(dobserved(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)

#onset
curve(dstart_time(x, par[1], par[2]),
      add = TRUE, col = "black", lwd = 2, n=res)

#cessation
curve(dend_time(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)

MAP_mean_s = beta_mean(par[1], par[2])*365
MAP_sd_s = beta_sd(par[1], par[2])*365
MAP_mean_d = beta_mean(par[3], par[4])*365
MAP_sd_d = beta_sd(par[3], par[3])*365

print(paste("Start: ", MAP_mean_s, "(+/-", MAP_sd_s, " 1 SD) Duration: ", MAP_mean_d, "(+/-", MAP_sd_d, " 1 SD)"))
}

doFitDistComparisonToEmpirical = function(fit,observed,nbreaks=25) {
hist(observed, breaks = seq(0,1,length.out=nbreaks), col = "purple", prob=T, main = "Fit of observed", xlab = "Time")

res = 1000
curve(dobserved(x, fit$estimate["alpha_s"], fit$estimate["beta_s"],
                        fit$estimate["alpha_d"], fit$estimate["beta_d"]),
      add = TRUE, col = "red", lwd = 2, n=res)

#onset
curve(dstart_time(x, fit$estimate["alpha_s"], fit$estimate["beta_s"]),
      add = TRUE, col = "black", lwd = 2, n=res)

#cessation
curve(dend_time(x, fit$estimate["alpha_s"], fit$estimate["beta_s"],
                        fit$estimate["alpha_d"], fit$estimate["beta_d"]),
      add = TRUE, col = "black", lwd = 2, n=res)

MLE_mean_s = beta_mean(fit$estimate["alpha_s"], fit$estimate["beta_s"])
MLE_sd_s = beta_sd(fit$estimate["alpha_s"], fit$estimate["beta_s"])
MLE_mean_d = beta_mean(fit$estimate["alpha_d"], fit$estimate["beta_d"])
MLE_sd_d = beta_sd(fit$estimate["alpha_d"], fit$estimate["beta_d"])

MLE_mean_s_SE = compute_se(fit$estimate["alpha_s"], fit$estimate["beta_s"], fit$sd["alpha_s"], fit$sd["beta_s"])$se_mean
MLE_sd_s_SE = compute_se(fit$estimate["alpha_s"], fit$estimate["beta_s"], fit$sd["alpha_s"], fit$sd["beta_s"])$se_sd
MLE_mean_d_SE = compute_se(fit$estimate["alpha_d"], fit$estimate["beta_d"], fit$sd["alpha_d"], fit$sd["beta_d"])$se_mean
MLE_sd_d_SE = compute_se(fit$estimate["alpha_d"], fit$estimate["beta_d"], fit$sd["alpha_d"], fit$sd["beta_d"])$se_sd

print(paste("Start: ", MLE_mean_s, "(+/-", MLE_sd_s, " 1 SD) Duration: ", MLE_mean_d, "(+/-", MLE_sd_d, " 1 SD)"))
}


#BB.m : data simulated under beta onset, beta duration
#BB.e : inferences based on BB MLE or BB MAP
#GP.m : data simulated under Gaussian process
#GP.e : inferences based on Stan under GP

makeSimulationInferenceResultsPlot = function(simData, trueMuO, trueMuD=NA, trueMuC, trueSDO, trueSDD=NA, estMuO, estMuD=NA, estMuC, estSDO, estSDD=NA, min=0, max=365, N=500, xRange=c(0,365), nBin=100, incC=0.1, BB.m=T, BB.e=F, GP.m=F, GP.e=T, pdfFile = NA, includeExtremes=T, includeHistogram=T) {

cat("Setting parameters\n")
if(!is.na(pdfFile)) {
	pdf(pdfFile)
}

if(GP.m + GP.e + BB.m + BB.e != 2) {
	stop("Only one model and only one estimate method are allowed, e.g.: BB.m && BB.e or BB.m && GP.e or GP.m && GP.e")
}

Ts = simData

mu_O = trueMuO
mu_C = trueMuC
mu_D = trueMuD
sigma = trueSDO
sigma_O = trueSDO
sigma_D = trueSDD

if(is.na(trueSDO)) {
	stop("The true standard deviation of the onset must be provided as input.")
}

if(is.na(trueMuO)) {
	stop("The true mean onset must be provided as input.")
}
else {
	mu_O = trueMuO
}

if(is.na(trueMuD) && !is.na(trueMuC)) {
	mu_D = mu_C - mu_O
}
else if(is.na(trueMuC) && !is.na(trueMuD)) {
	mu_C = mu_O + mu_D
}
else if(is.na(trueMuC) && is.na(trueMuD)) {
	stop("At least one of trueMuC or trueMuD must be provided")
}
if(is.na(trueSDD)) {
	sigma_D = 2* sigma_O
}

mu_O.e = estMuO
mu_C.e = estMuC
mu_D.e = estMuD
sigma.e = estSDO
sigma_O.e = estSDO
sigma_D.e = estSDD

if(is.na(estSDO)) {
	stop("The estimated standard deviation of the onset must be provided as input.")
}

if(is.na(estMuO)) {
	stop("The estimated mean onset must be provided as input.")
}
else {
	mu_O.e = estMuO
}

if(is.na(estMuD) && !is.na(estMuC)) {
	mu_D.e = mu_C.e - mu_O.e
}
else if(is.na(estMuC) && !is.na(estMuD)) {
	mu_C.e = mu_O.e + mu_D.e
}
else if(is.na(estMuC) && is.na(estMuD)) {
	stop("At least one of estMuC or estMuD must be provided")
}
if(is.na(estSDD)) {
	sigma_D.e = 2* sigma_O.e
}

#setting plotting limits
cat("Setting plotting limits\n")
xlim=xRange
inc = (xlim[2]-xlim[1])/((2/3)*nBin)
xlim[1] = xlim[1]-inc
xlim[2] = xlim[2]+inc
if(xlim[1] < min) { xlim[1] = min }
if(xlim[2] > max) { xlim[2] = max }
x = seq(xlim[1],xlim[2], inc)
xT = seq(xlim[1],xlim[2], incC)

#calculating theoretical PDFs
if(BB.m) {
cat("Calculating theoretical PDFs for BB\n")
T_theor.BB.t = dT.BB(xT, mu_O, sigma_O, mu_D, sigma_D, min, max)
O_theor.BB.t = dO.BB(xT, mu_O, sigma_O, min, max)
C_theor.BB.t = dC.BB(xT, mu_O, sigma_O, mu_D, sigma_D, min, max)
if(includeExtremes) {
	Ok1_theor.BB.t = dOk1.BB(xT, N, mu_O, sigma_O, min, max)
	cat("Calculating theoretical CkN (This may take a while to compute...)\n")
	CkN_theor.BB.t = dCkN.BB(xT, N, mu_O, sigma_O, mu_D, sigma_D, min, max)
}

EO.BB.t = E.O.BB(mu_O, sigma_O, min, max)
ET.BB.t = E.T.BB(mu_O, mu_D, min, max)
EC.BB.t = E.C.BB(mu_O, mu_D, min, max)
if(includeExtremes) {
	EOk1.BB.t = E.Ok1.BB(N, mu_O, sigma_O, min, max)
	ECkN.BB.t = E.CkN.BB(N, mu_O, sigma_O, mu_D, sigma_D, min, max)
}
#PIOk1.BB = PI.Ok1.BB(N, mu_O, sigma_O, min, max, alpha)
#PIO.BB = PI.O.BB(mu_O, sigma_O, min, max, alpha)
#PIT.BB = PI.T.BB(mu_O, sigma_O, mu_D, sigma_D, min, max, alpha)
#PIC.BB = PI.C.BB(mu_O, sigma_O, mu_D, sigma_D, min, max, alpha)
#PICkN.BB = PI.CkN.BB(N, mu_O, sigma_O, mu_D, sigma_D, min, max, alpha)
}
if(BB.e) {
T_theor.BB.e = dT.BB(xT, mu_O.e, sigma_O.e, mu_D.e, sigma_D.e, min, max)
O_theor.BB.e = dO.BB(xT, mu_O.e, sigma_O.e, min, max)
C_theor.BB.e = dC.BB(xT, mu_O.e, sigma_O.e, mu_D.e, sigma_D.e, min, max)
if(includeExtremes) {
	Ok1_theor.BB.e = dOk1.BB(xT, N, mu_O.e, sigma_O.e, min, max)
	cat("Calculating theoretical CkN (This may take a while to compute...)\n")
	CkN_theor.BB.e = dCkN.BB(xT, N, mu_O.e, sigma_O.e, mu_D.e, sigma_D.e, min, max)
}

EO.BB.e = E.O.BB(mu_O.e, sigma_O.e, min, max)
ET.BB.e = E.T.BB(mu_O.e, mu_D.e, min, max)
EC.BB.e = E.C.BB(mu_O.e, mu_D.e, min, max)
if(includeExtremes) {
	EOk1.BB.e = E.Ok1.BB(N, mu_O.e, sigma_O.e, min, max)
	ECkN.BB.e = E.CkN.BB(N, mu_O.e, sigma_O.e, mu_D.e, sigma_D.e, min, max)
}
#PIOk1.BB.e = PI.Ok1.BB(N, mu_O.e, sigma_O.e, min, max, alpha)
#PIO.BB.e = PI.O.BB(mu_O.e, sigma_O.e, min, max, alpha)
#PIT.BB.e = PI.T.BB(mu_O.e, sigma_O.e, mu_D.e, sigma_D.e, min, max, alpha)
#PIC.BB.e = PI.C.BB(mu_O.e, sigma_O.e, mu_D.e, sigma_D.e, min, max, alpha)
#PICkN.BB.e = PI.CkN.BB(N, mu_O.e, sigma_O.e, mu_D.e, sigma_D.e, min, max, alpha)
}

if(GP.m) {
cat("Calculating theoretical PDFs for GP\n")
	T_theor.GP.t = dT.GP(xT, mu_O, mu_C, sigma, min, max)
	O_theor.GP.t = dO.GP(xT, mu_O, sigma)
	C_theor.GP.t = dC.GP(xT, mu_C, sigma)
	if(includeExtremes) {
		Ok1_theor.GP.t = dOk1.GP(xT, N, mu_O, sigma)
		CkN_theor.GP.t = dCkN.GP(xT, N, mu_C, sigma)
	}
	
	EO.GP.t = E.O.GP(mu_O)
	ET.GP.t = E.T.GP(mu_O, mu_D)
	EC.GP.t = E.C.GP(mu_C)
	if(includeExtremes) {
		EOk1.GP.t = E.Ok1.GP(N, mu_O, sigma, min, max)
		ECkN.GP.t = E.CkN.GP(N, mu_C, sigma, min, max)
	}
#PIOk1.GP = PI.Ok1.GP(N, mu_O, sigma, alpha)
#PIO.GP = PI.O.GP(mu_O, sigma, alpha)
#PIT.GP = PI.T.GP(mu_O, mu_C, sigma, alpha)
#PIC.GP = PI.C.GP(mu_C, sigma, alpha)
#PICkN.GP = PI.CkN.GP(N, mu_C, sigma, alpha)
}
if(GP.e) {
	T_theor.GP.e = dT.GP(xT, mu_O.e, mu_C.e, sigma.e, min, max)
	O_theor.GP.e = dO.GP(xT, mu_O.e, sigma.e)
	C_theor.GP.e = dC.GP(xT, mu_C.e, sigma.e)
	if(includeExtremes) {
		Ok1_theor.GP.e = dOk1.GP(xT, N, mu_O.e, sigma.e)
		CkN_theor.GP.e = dCkN.GP(xT, N, mu_C.e, sigma.e)
	}

	EO.GP.e = E.O.GP(mu_O.e)
	ET.GP.e = E.T.GP(mu_O.e, mu_D.e)
	EC.GP.e = E.C.GP(mu_C.e)
	if(includeExtremes) {
		EOk1.GP.e = E.Ok1.GP(N, mu_O.e, sigma.e, min, max)
		ECkN.GP.e = E.CkN.GP(N, mu_C.e, sigma.e, min, max)
	}
#PIOk1.GP = PI.Ok1.GP(N, mu_O.e, sigma.e, alpha)
#PIO.GP = PI.O.GP(mu_O.e, sigma.e, alpha)
#PIT.GP = PI.T.GP(mu_O.e, mu_C.e, sigma.e, alpha)
#PIC.GP = PI.C.GP(mu_C.e, sigma.e, alpha)
#PICkN.GP = PI.CkN.GP(N, mu_C.e, sigma.e, alpha)
}

cat("Setting up plotting information\n")
par(mar = c(4, 4, 2, 3))  # Bottom, left, top, right
if(includeExtremes) {
	if(GP.m && GP.e) {
		ylim=range(T_theor.GP.t,O_theor.GP.t,C_theor.GP.t,Ok1_theor.GP.t,CkN_theor.GP.t, T_theor.GP.e,O_theor.GP.e,C_theor.GP.e,Ok1_theor.GP.e,CkN_theor.GP.e)
	}
	else if(BB.m && BB.e) { 
		ylim=range(T_theor.BB.t,O_theor.BB.t,C_theor.BB.t,Ok1_theor.BB.t,CkN_theor.BB.t, T_theor.BB.e,O_theor.BB.e,C_theor.BB.e,Ok1_theor.BB.e,CkN_theor.BB.e)
	}
	else if(BB.m && GP.e) {
		ylim=range(T_theor.GP.e,O_theor.GP.e,C_theor.GP.e,Ok1_theor.GP.e,CkN_theor.GP.e, T_theor.BB.t,O_theor.BB.t,C_theor.BB.t,Ok1_theor.BB.t,CkN_theor.BB.t)
	}
	else {
		stop("Only one model and only one estimate method are allowed, e.g.: BB.m && BB.e or BB.m && GP.e or GP.m && GP.e")
	}
}
else {
	if(GP.m && GP.e) {
		ylim=range(T_theor.GP.t,O_theor.GP.t,C_theor.GP.t,T_theor.GP.e,O_theor.GP.e,C_theor.GP.e)
	}
	else if(BB.m && BB.e) { 
		ylim=range(T_theor.BB.t,O_theor.BB.t,C_theor.BB.t,T_theor.BB.e,O_theor.BB.e,C_theor.BB.e)
	}
	else if(BB.m && GP.e) {
		ylim=range(T_theor.GP.e,O_theor.GP.e,C_theor.GP.e,T_theor.BB.t,O_theor.BB.t,C_theor.BB.t)
	}
	else {
		stop("Only one model and only one estimate method are allowed, e.g.: BB.m && BB.e or BB.m && GP.e or GP.m && GP.e")
	}
}
#draw histogram of simulated data
if(includeHistogram) { hist(Ts, breaks=x, prob=TRUE, col=rgb(1,0,1,0.5),  ylim=ylim, xlab="Day of Year", main=NULL) } 
else { plot(x = NULL, y = NULL, xlim = c(min(x), max(x)), ylim = ylim, type = "n", xlab="Day of Year", ylab="Density") } 

#draw polygons of true distributions
if(BB.m) {
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(T_theor.BB.t)),  
        col = rgb(1,0,1, 0.4), border = NA)
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(O_theor.BB.t)),  
        col = rgb(1,0,0, 0.4), border = NA)
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(C_theor.BB.t)),  
        col = rgb(0,0,1, 0.4), border = NA)
	if(includeExtremes) {
		polygon(c(xT, rev(xT)),              
        		c(rep(0, length(xT)), rev(Ok1_theor.BB.t)),  
        		col = rgb(1,1,0, 0.4), border = NA)
		polygon(c(xT, rev(xT)),              
        		c(rep(0, length(xT)), rev(CkN_theor.BB.t)),  
        		col = rgb(0,1,1, 0.4), border = NA)
	}
}
if(GP.m) {
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(T_theor.GP.t)),  
        col = rgb(1,0,1, 0.4), border = NA)
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(O_theor.GP.t)),  
        col = rgb(1,0,0, 0.4), border = NA)
polygon(c(xT, rev(xT)),              
        c(rep(0, length(xT)), rev(C_theor.GP.t)),  
        col = rgb(0,0,1, 0.4), border = NA)
	if(includeExtremes) {
		polygon(c(xT, rev(xT)),              
        		c(rep(0, length(xT)), rev(Ok1_theor.GP.t)),  
        		col = rgb(1,1,0, 0.4), border = NA)
		polygon(c(xT, rev(xT)),              
        		c(rep(0, length(xT)), rev(CkN_theor.GP.t)),  
        		col = rgb(0,1,1, 0.4), border = NA)
	}
}


#draw solid lines of inferred distributions
if(GP.e) {
	points(xT, O_theor.GP.e, type='l',col="red", lwd=1)
	points(xT, C_theor.GP.e, type='l',col="blue", lwd=1)
	points(xT, T_theor.GP.e, type='l',col="purple", lwd=2)
	if(includeExtremes) {
		points(xT, Ok1_theor.GP.e, type='l',col="gold4", lwd=1)
		points(xT, CkN_theor.GP.e, type='l',col="cyan4", lwd=1)
	}
}

if(BB.e) {
	points(xT, O_theor.BB.e, type='l',col="red", lwd=1)
	points(xT, C_theor.BB.e, type='l',col="blue", lwd=1)
	points(xT, T_theor.BB.e, type='l',col="purple", lwd=2)
	if(includeExtremes) {
		points(xT, Ok1_theor.BB.e, type='l',col="gold4", lwd=1)
		points(xT, CkN_theor.BB.e, type='l',col="cyan4", lwd=1)
	}
}

#cat("Peak\n")
#peak = getPeak.T.BB(mu_O, sigma_O, mu_D, sigma_D, min, max)
#ind = which.max(T_theor.BB)
#points(peak, T_theor[ind], pch=16, cex=2.5, col="purple")
#points(peak, T_theor[ind], pch=1, cex=2.5)

# Plot expected values and ranges
#pin_y <- par("pin")[2]              # Height of plot in inches
#usr_y <- par("usr")[4] - par("usr")[3]  # y-axis range in user units
#y_units_per_inch <- usr_y / pin_y
#lwd <- 10
#inch_per_lwd <- 1 / 96 
#line_thickness_y_units <- lwd * inch_per_lwd * y_units_per_inch
#posYInc = line_thickness_y_units / 3
#
#segments(x0 = PIOk1.GP[1], y0 = -5*posYInc, x1 = PIOk1.GP[2], y1 = -5*posYInc, col = "gold4", lwd = 2)
#points(x=EOk1.GP, y=-5*posYInc, pch=16, cex=1)
#segments(x0 = PIO.GP[1], y0 = -4*posYInc, x1 = PIO.GP[2], y1 = -4*posYInc, col = "red", lwd = 2)
#points(x=EO.GP, y=-4*posYInc, pch=16, cex=1)
#segments(x0 = PIT.GP[1], y0 = -3*posYInc, x1 = PIT.GP[2], y1 =  -3*posYInc, col = "purple", lwd = 2)
#points(x=ET.GP, y=-3*posYInc, pch=16, cex=1)
#segments(x0 = PIC.GP[1], y0 = -2*posYInc, x1 = PIC.GP[2], y1 =  -2*posYInc, col = "blue", lwd = 2)
#points(x=EC.GP, y=-2*posYInc, pch=16, cex=1)
#segments(x0 = PICkN.GP[1], y0 = -5*posYInc, x1 = PICkN.GP[2], y1 = -5*posYInc, col = "cyan4", lwd = 2)
#points(x=ECkN.GP, y=-5*posYInc, pch=16, cex=1)
if(BB.m) {
	abline(v = EO.BB.t, col="red", lwd=1)
	abline(v = ET.BB.t, col="purple", lwd=1)
	abline(v = EC.BB.t, col="blue", lwd=1)
	if(includeExtremes) {
		abline(v = EOk1.BB.t, col="yellow", lwd=1)
		abline(v = ECkN.BB.t, col="cyan", lwd=1)
	}
}
if(GP.m) {
	abline(v = EC.GP.t, col="red", lwd=0.75)
	abline(v = EO.GP.t, col="purple", lwd=0.75)
	abline(v = ET.GP.t, col="blue", lwd=0.75)
	if(includeExtremes) {
		abline(v = EOk1.GP.t, col="yellow", lwd=0.75)
		abline(v = ECkN.GP.t, col="cyan", lwd=0.75)
	}
}

if(BB.e) {
	abline(v = EO.BB.e, col="black", lwd=0.75)
	abline(v = ET.BB.e, col="black", lwd=0.75)
	abline(v = EC.BB.e, col="black", lwd=0.75)
	if(includeExtremes) {
		abline(v = EOk1.BB.e, col="black", lwd=0.75)
		abline(v = ECkN.BB.e, col="black", lwd=0.75)
	}
}
if(GP.e) {
	abline(v = EC.GP.e, col="black", lwd=0.75)
	abline(v = EO.GP.e, col="black", lwd=0.75)
	abline(v = ET.GP.e, col="black", lwd=0.75)
	if(includeExtremes) {
		abline(v = EOk1.GP.e, col="black", lwd=0.75)
		abline(v = ECkN.GP.e, col="black", lwd=0.75)
	}
}

if(!is.na(pdfFile)) {
	dev.off()
}


}
