run_AnalysisParadox = function(replicates=1000, outputFile="output.txt", scenarioNumber, append=F, seed=12345) {
	library(quantreg)
	library(posterior)
	library(cmdstanr)
	library(dplyr)
	source("phenologySimulation.R")
	source("phenologyInference.R")

	set.seed(seed)
	output = sprintf("Replicate\tSampleSize\tScenarioNumber\tTrue_Slope_meanOnset\tTrue_Intercept_meanOnset\tTrue_Anchor_meanOnset\tTrue_Slope_meanDuration\tTrue_Intercept_meanDuration\tTrue_Anchor_meanDuration\tTrue_Sigma\tTheor_Slope_meanObserved\tTheor_Intercept_meanObserved\tTheor_Slope_meanCessation\tTheor_Intercept_meanCessation\tTrueObs_meanOnset\tTrueObs_meanDuration\tTheor_meanOnset\tTheor_meanDuration\tHyper_Mean_Slope_meanOnset\tHyper_SD_Slope_meanOnset\tHyper_Mean_Slope_meanDuration\tHyper_SD_Slope_meanDuration\tHyper_Mean_Anchor_meanOnset\tHyper_SD_Anchor_meanOnset\tHyper_Mean_Anchor_meanDuration\tHyper_SD_Anchor_meanDuration\tHyper_Mean_Sigma\tHyper_SD_Sigma\tEst_SR_Slope_meanObserved\tEst_SR_Intercept_meanObserved\tEst_QR_Slope_meanQ10\tEst_QR_Intercept_meanQ10\tEst_QR_Slope_meanQ50\tEst_QR_Intercept_meanQ50\tEst_QR_Slope_meanQ90\tEst_QR_Intercept_meanQ90\tEst_Stan_Slope_meanOnset\tEst_Stan_Slope_meanOnset_q2.5\tEst_Stan_Slope_meanOnset_q97.5\tEst_Stan_Intercept_meanOnset\tEst_Stan_Intercept_meanOnset_q2.5\tEst_Stan_Intercept_meanOnset_q97.5\tEst_Stan_Anchor_meanOnset\tEst_Stan_Anchor_meanOnset_q2.5\tEst_Stan_Anchor_meanOnset_q97.5\tEst_Stan_Slope_meanDuration\tEst_Stan_Slope_meanDuration_q2.5\tEst_Stan_Slope_meanDuration_q97.5\tEst_Stan_Intercept_meanDuration\tEst_Stan_Intercept_meanDuration_q2.5\tEst_Stan_Intercept_meanDuration_q97.5\tEst_Stan_Anchor_meanDuration\tEst_Stan_Anchor_meanDuration_q2.5\tEst_Stan_Anchor_meanDuration_q97.5\tEst_Stan_Sigma\tEst_Stan_Sigma_q2.5\tEst_Stan_Sigma_q97.5\tNumber_Divergences\n")
	cat(output ,file=outputFile,append=append)
	#cat(paste("The headers are: ", output, "\n"))

	#set up the partition proportion
	prop = 0.5

	#repeats before strigent run
	maxRej = 5

	#set the sample sizes
	#minSS=60
	#maxSS = 1000
	SS = 1000

	#set the range of responses
	minResponse = 0
	maxResponse = 365
	rangeResponse = maxResponse-minResponse
	rangeMid = rangeResponse/2

	#set the max and min mean duration
	maxDuration = 90
	minDuration = 30
	rangeDuration = maxDuration-minDuration 

	#set the min and max mean onset (from Callinger et al. 2013)
	minOnset = 75
	maxOnset = 225
	rangeOnset = maxOnset-minOnset

	#set the range in temperature (from Callinger et al. 2013)
	minC = -2
	maxC = 24
	rangeC = maxC-minC
	meanCovariate = minC + rangeC / 2	#based on mean of uniform random variable between minC and maxC (uniform sampling)

	#set the min and max duration slopes
	maxDurationSlope = rangeDuration / rangeC
	minDurationSlope = maxDurationSlope / 5

	#set the min and max onset slopes
	maxOnsetSlope = rangeOnset / rangeC
	#minOnsetSlope = maxOnsetSlope / 5
	minOnsetSlope = 0.75

	#set the cushion, which defines the separation between the paradox thresholds
	cushion = minDurationSlope / 2

	#set the SD on onset and cessation
	sigma = 7

	#set up the constant hyperparameters
	H_SD_SlopeO = 2 * 2.83 #set to twice the SD on phenological sensitivities from Callinger et al. 2013
	H_SD_SlopeD = 2 * (maxDurationSlope - minDurationSlope) / sqrt(12) #set to twice the SD of the uniform distribution between min and max slopes

	#H_SD_SlopeO = 10
	#H_SD_SlopeD = 10

	#H_SD_SlopeO = 5
	#H_SD_SlopeD = 3
	#H_SD_Anchor_meanOnset = 5		
	#H_SD_Anchor_meanDuration = 14 
	#H_SD_Anchor_meanOnset = 14		
	#H_SD_Anchor_meanDuration = 14 
	H_SD_Anchor_meanOnset = 14
	H_SD_Anchor_meanDuration = 14
	H_SD_Anchor_meanCessation = 14
	H_Mean_Sigma = 7
	H_SD_Sigma = 3.5

	it = 1
	nRej = 0
	while(it <= replicates) {
		cat(paste("Starting replicate ", it, " with ", nRej, " rejected Stan runs previous. \n"))
		#set sample size
		#SS = round(runif(1, minSS, maxSS))
		SS = 1000	#sample size is readjusted after partitioning, so must refresh to original size (or set the size to the partition size and not refresh)

		cat(paste("Picking scenario number: ", scenarioNumber, "\n"))
		#calculate slopes
		#calculate max range of values and set onset and duration anchors
		#+++
		if(scenarioNumber == 1) {
			slopeD = runif(1, minDurationSlope, maxDurationSlope)
			slopeO = runif(1, minOnsetSlope, maxOnsetSlope)

			durationStart = minDuration + 20
		}

		#+-+
		if(scenarioNumber == 2) {
			slopeD = -runif(1, minDurationSlope, maxDurationSlope)
			minSO = -slopeD/2 + cushion
			if(minSO > maxOnsetSlope || minSO < 0) { 
				warning("The minimum allowable slope for scenario 2 was greater than the maximum allowable slope.") 
				next
			}
			slopeO = runif(1, minSO, maxOnsetSlope)

			durationStart = maxDuration
		}

		#+--	Paradox
		if(scenarioNumber == 3) {
			#slopeD = -runif(1, minDurationSlope, maxDurationSlope)
			slopeD = -runif(1, 2*minOnsetSlope, maxDurationSlope)
			maxSO = -slopeD/2 - cushion
			cat(paste("minDurationSlope: ", minDurationSlope, " maxDurationSlope: ", maxDurationSlope, " minOnsetSlope: ", minOnsetSlope, " maxSO: ", maxSO, " maxOnsetSlope: ", maxOnsetSlope, " Picked slopeD: ", slopeD, "\n"))
			if(maxSO < minOnsetSlope ) { 
				warning(paste("For scenario 3, max onset slope, ", maxSO, " is less than min onset slope, ", minOnsetSlope, ".")) 
				#stop("EEK")
				next
			}
			if(maxSO < 0 ) { 
				warning("For scenario 3, max onset slope is negative when it should be positive.") 
				next
			}
			slopeO = runif(1, minOnsetSlope, maxSO)

			durationStart = maxDuration
		}

		#---
		if(scenarioNumber == 4) {
			slopeD = -runif(1, minDurationSlope, maxDurationSlope)
			slopeO = -runif(1, minOnsetSlope, maxOnsetSlope)

			durationStart = maxDuration
		}

		#-+-
		if(scenarioNumber == 5) {
			slopeD = runif(1, minDurationSlope, maxDurationSlope)
			maxSO = -slopeD/2 - cushion
			if(maxSO < -maxOnsetSlope) {
				warning(paste("For scenario 5, the maximum onset slope ", maxSO, " is less than the minimum onset slope, ", (-maxOnsetSlope), "."))
				next
			}
			slopeO = runif(1, -maxOnsetSlope, maxSO)

			durationStart = minDuration + 20
		}

		#-++
		if(scenarioNumber == 6) {
			slopeD = runif(1, minDurationSlope, maxDurationSlope)
			minSO = -slopeD/2 + cushion
			if(minSO > -minOnsetSlope) {
				warning(paste("For scenario 6, the minimum onset slope ", minSO, " is greater than the maximum onset slope, ", (-minOnsetSlope), "."))
				next
			}
			slopeO = runif(1, minSO, -minOnsetSlope)

			durationStart = minDuration + 20
		}


		sl = 0
		su = sl + durationStart
		el = sl + slopeO * rangeC
		eu = el + durationStart + slopeD * rangeC

		minR = min(sl,su,el,eu)
		maxR = max(sl,su,el,eu)

		rangeR = maxR - minR
		if(rangeR < 0) {
			stop("The predicted range in the response is negative.")
		}

		cat(paste("The range between the minimum point and maximum point is expected to be ", rangeR, "\n"))
		cat(paste("The range between the minimum response and the maximum response is ", 2*rangeMid, "\n"))
		print(c(sl,su,el,eu))

		#if(sl < minResponse || su < minResponse || el < minResponse || eu < minResponse || sl > maxResponse || su > maxResponse || el > maxResponse || eu > maxResponse) {
		#next
		#}

		#meanO = rangeMid - rangeR/2
		#if(meanO < minOnset) {
		#stop("The predicted mean onset is less than the allowed minimum mean onset.")
		#}
		#if(meanO > maxOnset) {
		#stop("The predicted mean onset is greater than the allowed maximum mean onset.")
		#}

		meanO = rangeResponse/2

		meanD = ((su - sl) + (eu - el) ) / 2
		if(meanD < minDuration) {
			stop("The predicted mean duration is less than the allowed minimum mean duration.")
		}
		if(meanD > maxDuration) {
			stop("The predicted mean duration is greater than the allowed maximum mean duration.")
		}

		cat(paste("Scenario ", scenarioNumber, " with onset slope ", slopeO, " and duration slope ", slopeD, " and mean onset ", meanO, " and mean duration ", meanD, "\n"))

		interceptO = meanO - meanCovariate * slopeO
		interceptD = meanD - meanCovariate * slopeD

		#simulate data under known parameters
		cat("Simulating data under GP.\n")
		sim = simulateCovariate(n=SS, slopeO=slopeO, interceptO=interceptO, sigma=sigma, slopeD=slopeD, interceptD=interceptD, minCovariate=minC, maxCovariate=maxC)

		y = sim$Ts
		x = sim$X

		#set partition for prior and for HMC run
		n = length(y)
		train_indices <- sample(n, size = floor(prop * n), replace=F)

		SS = floor(prop*n)

		y_rq = y[-train_indices]	#30% for quantile regression to set the priors
		x_rq = x[-train_indices]

		sim$Ts = y[train_indices]	#70% for stan analysis
		sim$X = x[train_indices]
		sim$O = sim$O[train_indices]
		sim$C = sim$C[train_indices]

		#print(length(sim$Ts))
		#print(length(sim$X))


		#run quantile regression
		resQ10 = rq(y_rq ~ x_rq, tau=0.1)
		resQ50 = rq(y_rq ~ x_rq, tau=0.5) 	#could be done together!
		resQ90 = rq(y_rq ~ x_rq, tau=0.9) 	#could be done together!
		#resQ10 = rq(y ~ x, tau=0.1)
		#resQ50 = rq(y ~ x, tau=0.5) 	#could be done together!
		#resQ90 = rq(y ~ x, tau=0.9) 	#could be done together!

		cat(paste("Generating plots"))
		fit <- lm(sim$Ts ~ sim$X)
		slopeO_SR = fit$coefficients[2]
		text = paste("Rep: ", it, ": Scenario ", scenarioNumber, " Rejects: ", nRej, "\nSlope Onset: ", slopeO, "\nSlope Duration: ", slopeD, "\nMean Duration: ", meanD, "\nMean Onset: ", meanO, "\nSlope observed: ", slopeO_SR, "\nTheor slope observed: ", slopeO + 0.5*slopeD) 

		plot(sim$X, sim$Ts, ylim=c(minResponse, maxResponse),col="purple", pch=16)
		points(sim$X, sim$O, col="blue")
		points(sim$X, sim$C, col="red")
		segments(sim$X, sim$O, sim$X, sim$C)
		abline(fit, col = "purple", lwd = 2)
		points(mean(sim$X), meanO+meanD/2, col="purple", pch = 16, cex = 3)
		text(mean(sim$X), 0.99 * maxResponse, text, pos=1)
		#name <- readline(prompt = "Push Enter to continue.")

		#set the prior hyperparameters based on QR results 
		#H_Mean_SlopeO = slopeO
		#H_Mean_SlopeD = slopeD
		#H_Mean_Anchor_meanOnset = meanO
		#H_Mean_Anchor_meanDuration = meanD
		H_Mean_SlopeO = resQ10$coefficients[2]
		H_Mean_SlopeD = resQ90$coefficients[2] - resQ10$coefficients[2]
		H_Mean_Anchor_meanOnset = resQ10$coefficients[1] + resQ10$coefficients[2] * mean(sim$X)					#Empirical
		H_Mean_Anchor_meanDuration = resQ90$coefficients[1] - resQ10$coefficients[1] + H_Mean_SlopeD * mean(sim$X)	#Empirical
		H_Mean_Anchor_meanCessation = resQ90$coefficients[1] + resQ90$coefficients[2] * mean(sim$X)
		#H_Mean_Anchor_meanOnset = resQ10$coefficients[1] + resQ10$coefficients[2] * meanCoviariate						#theory based on uniform sampling done during simulation - won't have
		#H_Mean_Anchor_meanDuration = resQ90$coefficients[1] - resQ10$coefficients[1] + H_Mean_SlopeD * meanCoviariate	#theory based on uniform sampling done during simulation - won't have
		#H_SD_SlopeO = 7
		#H_SD_SlopeD = 7
		#H_SD_Anchor_meanOnset = 15		#somewhat based on Park et al 2024 absolute error.
		#H_SD_Anchor_meanDuration = 15
		#H_SD_Sigma = 3.5
		#H_Mean_Sigma = 7

		cat(paste("H_Mean_SlopeO:", H_Mean_SlopeO, " H_Mean_SlopeD: ", H_Mean_SlopeD, " H_Mean_Anchor_meanO: ", H_Mean_Anchor_meanOnset, " H_Mean_Anchor_meanDuration ", H_Mean_Anchor_meanDuration, ", H_SD_SlopeO: ", H_SD_SlopeO, ", H_SD_SlopeD: ", H_SD_SlopeD, " anchor sds: ", H_SD_Anchor_meanOnset, "\n"))

		#set up the hyperparameters for input
		hyperSlopeO = data.frame(H_Mean_SlopeO,H_SD_SlopeO)
		hyperAnchorO = c(H_Mean_Anchor_meanOnset,H_SD_Anchor_meanOnset)
		hyperSlopeD = data.frame(H_Mean_SlopeD,H_SD_SlopeD)
		hyperAnchorD = c(H_Mean_Anchor_meanDuration,H_SD_Anchor_meanDuration)
		hyperAnchorC = c(H_Mean_Anchor_meanCessation, H_SD_Anchor_meanCessation)
		hyperSigma = c(H_Mean_Sigma, H_SD_Sigma)

		cat(paste("Setting hyperparameters for example the mean anchor for onset:", H_Mean_Anchor_meanOnset, "\n"))
		cat("Attempting to conduct a Stan analysis.\n")

		resGP = tryCatch({
			if(nRej < maxRej) {
				runStanPhenology(type="full",
								 responseData=sim$Ts,
								 onsetCovariateData=data.frame(meanSpringTemperature = sim$X),
								 durationCovariateData=data.frame(meanSpringTemperature=sim$X),
								 onsetHyperBeta=hyperSlopeO,
								 onsetHyperAnchor=hyperAnchorO,
								 durationHyperBeta=hyperSlopeD,
								 durationHyperAnchor=hyperAnchorD,
								 cessationHyperAnchor=hyperAnchorC,			#no longer used
								 sigmaHyper=hyperSigma,
								 minResponse=minResponse,
								 maxResponse=maxResponse,
								 maxDiv=0,
								 setStringent=F,
								 runMap=F,
								 processExtremes=F,
								 N=NA,
								 threshApprox=NA,
								 partitionDataForPriors=F
				)

			}


			else {
				runStanPhenology(type="full",
								 hyperparams_noCovariates=NA,
								 responseData=sim$Ts,
								 onsetCovariateData=data.frame(meanSpringTemperature = sim$X),
								 durationCovariateData=data.frame(meanSpringTemperature=sim$X),
								 onsetHyperBeta=hyperSlopeO,
								 onsetHyperAnchor=hyperAnchorO,
								 durationHyperBeta=hyperSlopeD,
								 durationHyperAnchor=hyperAnchorD,
								 cessationHyperAnchor=hyperAnchorC,			#no longer used
								 sigmaHyper=hyperSigma,
								 minResponse=minResponse,
								 maxResponse=maxResponse,
								 maxDiv=0,
								 setStringent=T,
								 runMap=F,
								 processExtremes=F,
								 N=NA,
								 threshApprox=NA,
								 partitionDataForPriors=F
				)

			}
		}, error = function(e) {
			message <- conditionMessage(e)
			cat(paste("ERROR here: ", message, "\n"))
			list(error=T, 
				 error_m="Stan failed to run.")

		})

		if(resGP$error) { 
			cat(paste("ERROR: ", resGP$error, "\n"))
			#stop("Stan error")
			nRej = nRej + 1
			next
		}

		nDiv = sum(resGP$result$diagnostic_summary()$num_divergent)
		if(nDiv>0) { 
			nRej = nRej + 1
			next 
		}

		betaSummaryO = resGP$result$summary(variables="beta_O[1]",posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()
		betaSummaryD = resGP$result$summary(variables="beta_D[1]",posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()
		summary = resGP$result$summary(variables = c("anchor_O", "anchor_D", "alpha_O", "alpha_D", "sigma"),posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()

		#standard regression
		resSR = lm(y ~ x)

		cat("Printing results.\n")
		#print("replicate number")
		#print(									it)
		#print("sample size")
		#print(									SS)
		#print("scenario number")
		#print(									scenarioNumber)
		#print("true beta_o value")
		#print(									slopeO)
		#print("true alpha_o value")
		#print(									interceptO)
		#print("true mean_o value")
		#print(									meanO)
		#print("true beta_d value")
		#print(									slopeD)
		#print("true alpha_d value")
		#print(									interceptD)
		#print("true mean_d value")
		#print(									meanD)
		#print("True_Sigma")
		#print(									sigma)
		#print("theoretical beta_observed	(in theory, this is beta_o + beta_d/2)")
		#print(									slopeO + slopeD/2)
		#print("theoretical alpha_observed	(in theory, this is alpha_o + alpha_d/2)")
		#print(									interceptO + interceptD/2)
		#print("theoretical beta_cessation")
		#print(									slopeO + slopeD)
		#print("theoretical alpha_cessation")
		#print(									interceptO + interceptD)
		#print("true observed mean onset over the time interval (mean(sim$t_start))")
		#print(									mean(sim$O))
		#print("true observed mean duration over the time interval (mean(sim$t_end - sim$t_start))")
		#print(									mean(sim$D))
		#print("theor mean onset over the time interval alpha_o + beta_o * (x_min + x_max) /2 ")
		#print(									interceptO + minC*slopeO + mean(sim$X)*slopeO)
		#print("theor mean duration over the time interval alpha_d + beta_d * (x_min + x_max) /2")
		#print(									interceptD + minC*slopeD + mean(sim$X)*slopeD)
		#print("hyperparameters (mean, sd) for beta_o bias mean")
		#print(									H_Mean_SlopeO)
		#print("hyperparameters (mean, sd) for beta_o bias SD")
		#print(									H_SD_SlopeO)
		#print("hyperparameters (mean, sd) for beta_d bias mean")
		#print(									H_Mean_SlopeD)
		#print("hyperparameters (mean, sd) for beta_d bias SD")
		#print(									H_SD_SlopeD)
		#print("hyperparameters (mean, sd) for mean_o bias mean")
		#print(									H_Mean_Anchor_meanOnset)
		#print("hyperparameters (mean, sd) for mean_o bias SD")
		#print(									H_SD_Anchor_meanOnset)
		#print("hyperparameters (mean, sd) for mean_d bias mean")
		#print(									H_Mean_Anchor_meanDuration)
		#print("hyperparameters (mean, sd) for mean_d bias SD")
		#print(									H_SD_Anchor_meanDuration)
		#print("hyperparameters (mean, sd) for sigma bias mean")
		#print(									H_Mean_Sigma)
		#print("hyperparameters (mean, sd) for sigma bias SD")
		#print(									H_SD_Sigma)
		#print("inferred beta_observed SR")
		#print(									resSR$coefficients[2])
		#print("inferred alpha_observed SR")
		#print(									resSR$coefficients[1])
		#print("inferred beta_observed QR q10")
		#print(									resQ10$coefficients[2])
		#print("inferred alpha_observed QR q10")
		#print(									resQ10$coefficients[1])
		#print("inferred beta_observed QR q50")
		#print(									resQ50$coefficients[2])
		#print("inferred alpha_observed QR q50")
		#print(									resQ50$coefficients[1])
		#print("inferred beta_observed QR q90")
		#print(									resQ90$coefficients[2])
		#print("inferred alpha_observed QR q90")
		#print(									resQ90$coefficients[1])
		#print("inferred beta_o GP")
		#print(									betaSummaryO$mean[1])
		#print("inferred beta_o q2.5 GP")
		#print(									betaSummaryO$q2.5[1])
		#print("inferred beta_o q97.5 GP")
		#print(									betaSummaryO$q97.5[1])
		#print("inferred alpha_o GP")
		#print(									summary[summary$variable=="alpha_O",]$mean)
		#print("inferred alpha_o q2.5 GP")
		#print(									summary[summary$variable=="alpha_O",]$q2.5)
		#print("inferred alpha_o q97.5 GP")
		#print(									summary[summary$variable=="alpha_O",]$q97.5)
		#print("inferred mean_o GP")
		#print(									summary[summary$variable=="anchor_O",]$mean)
		#print("inferred mean_o q2.5 GP")
		#print(									summary[summary$variable=="anchor_O",]$q2.5)
		#print("inferred mean_o q97.5 GP")
		#print(									summary[summary$variable=="anchor_O",]$q97.5)
		#print("inferred beta_d GP")
		#print(									betaSummaryD$mean[1])
		#print("inferred beta_d q2.5 GP")
		#print(									betaSummaryD$q2.5[1])
		#print("inferred beta_d q97.5 GP")
		#print(									betaSummaryD$q97.5[1])
		#print("inferred alpha_d GP")
		#print(									summary[summary$variable=="alpha_D",]$mean)
		#print("inferred alpha_d q2.5 GP")
		#print(									summary[summary$variable=="alpha_D",]$q2.5)
		#print("inferred alpha_d q97.5 GP")
		#print(									summary[summary$variable=="alpha_D",]$q97.5)
		#print("inferred mean_d GP")
		#print(									summary[summary$variable=="anchor_D",]$mean)
		#print("inferred mean_d q2.5 GP")
		#print(									summary[summary$variable=="anchor_D",]$q2.5)
		#print("inferred mean_d q97.5 GP")
		#print(									summary[summary$variable=="anchor_D",]$q97.5)
		#print("inferred sigma GP")
		#print(									summary[summary$variable=="sigma",]$mean)
		#print("inferred sigma q2.5 GP")
		#print(										summary[summary$variable=="sigma",]$q2.5)
		#print("inferred sigma q97.5 GP")
		#print(										summary[summary$variable=="sigma",]$q97.5)
		#print("number of divergences")
		#print(										nDiv)

		pRes = tryCatch({
			cat(paste("Printing to ", outputFile, "\n\n"))
			output = sprintf("%d\t%d\t%d\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%d\n",
							 #replicate number
							 it,
							 #sample size
							 SS,
							 #scenario number
							 scenarioNumber,
							 #true beta_o value
							 slopeO,
							 #true alpha_o value
							 interceptO,
							 #true mean_o value
							 meanO,
							 #true beta_d value
							 slopeD,
							 #true alpha_d value
							 interceptD,
							 #true mean_d value
							 meanD,
							 #True_Sigma,
							 sigma,
							 #theoretical beta_observed	(in theory, this is beta_o + beta_d/2)
							 slopeO + slopeD/2,
							 #theoretical alpha_observed	(in theory, this is alpha_o + alpha_d/2)
							 interceptO + interceptD/2,
							 #theoretical beta_cessation
							 slopeO + slopeD,
							 #theoretical alpha_cessation
							 interceptO + interceptD,
							 #true observed mean onset over the time interval (mean(sim$t_start))
							 mean(sim$O),
							 #true observed mean duration over the time interval (mean(sim$t_end - sim$t_start))
							 mean(sim$D),
							 #theor mean onset over the time interval alpha_o + beta_o * (x_min + x_max) /2 
							 interceptO + minC*slopeO + mean(sim$X)*slopeO,
							 #theor mean duration over the time interval alpha_d + beta_d * (x_min + x_max) /2
							 interceptD + minC*slopeD + mean(sim$X)*slopeD,
							 #hyperparameters (mean, sd) for beta_o bias mean
							 H_Mean_SlopeO,
							 #hyperparameters (mean, sd) for beta_o bias SD
							 H_SD_SlopeO,
							 #hyperparameters (mean, sd) for beta_d bias mean
							 H_Mean_SlopeD,
							 #hyperparameters (mean, sd) for beta_d bias SD
							 H_SD_SlopeD,
							 #hyperparameters (mean, sd) for mean_o bias mean
							 H_Mean_Anchor_meanOnset,
							 #hyperparameters (mean, sd) for mean_o bias SD
							 H_SD_Anchor_meanOnset,
							 #hyperparameters (mean, sd) for mean_d bias mean
							 H_Mean_Anchor_meanDuration,
							 #hyperparameters (mean, sd) for mean_d bias SD
							 H_SD_Anchor_meanDuration,
							 #hyperparameters (mean, sd) for sigma bias mean
							 H_Mean_Sigma,
							 #hyperparameters (mean, sd) for sigma bias SD
							 H_SD_Sigma,
							 #inferred beta_observed SR
							 resSR$coefficients[2],
							 #inferred alpha_observed SR
							 resSR$coefficients[1],
							 #inferred beta_observed QR q10
							 resQ10$coefficients[2],
							 #inferred alpha_observed QR q10
							 resQ10$coefficients[1],
							 #inferred beta_observed QR q50
							 resQ50$coefficients[2],
							 #inferred alpha_observed QR q50
							 resQ50$coefficients[1],
							 #inferred beta_observed QR q90
							 resQ90$coefficients[2],
							 #inferred alpha_observed QR q90
							 resQ90$coefficients[1],
							 #inferred beta_o GP
							 betaSummaryO$mean[1],
							 #inferred beta_o q2.5 GP
							 betaSummaryO$q2.5[1],
							 #inferred beta_o q97.5 GP
							 betaSummaryO$q97.5[1],
							 #inferred alpha_o GP
							 summary[summary$variable=="alpha_O",]$mean,
							 #inferred alpha_o q2.5 GP
							 summary[summary$variable=="alpha_O",]$q2.5,
							 #inferred alpha_o q97.5 GP
							 summary[summary$variable=="alpha_O",]$q97.5,
							 #inferred mean_o GP
							 summary[summary$variable=="anchor_O",]$mean,
							 #inferred mean_o q2.5 GP
							 summary[summary$variable=="anchor_O",]$q2.5,
							 #inferred mean_o q97.5 GP
							 summary[summary$variable=="anchor_O",]$q97.5,
							 #inferred beta_d GP
							 betaSummaryD$mean[1],
							 #inferred beta_d q2.5 GP
							 betaSummaryD$q2.5[1],
							 #inferred beta_d q97.5 GP
							 betaSummaryD$q97.5[1],
							 #inferred alpha_d GP
							 summary[summary$variable=="alpha_D",]$mean,
							 #inferred alpha_d q2.5 GP
							 summary[summary$variable=="alpha_D",]$q2.5,
							 #inferred alpha_d q97.5 GP
							 summary[summary$variable=="alpha_D",]$q97.5,
							 #inferred mean_d GP
							 summary[summary$variable=="anchor_D",]$mean,
							 #inferred mean_d q2.5 GP
							 summary[summary$variable=="anchor_D",]$q2.5,
							 #inferred mean_d q97.5 GP
							 summary[summary$variable=="anchor_D",]$q97.5,
							 #inferred sigma GP,
							 summary[summary$variable=="sigma",]$mean,
							 #inferred sigma q2.5 GP,
							 summary[summary$variable=="sigma",]$q2.5,
							 #inferred sigma q97.5 GP,
							 summary[summary$variable=="sigma",]$q97.5,
							 #number of divergences
							 nDiv
			)
			cat(output,file=outputFile,append=T)
		}, error = function(e) {
			message <- conditionMessage(e)
			print(paste("Print statement failed. Retrying. Error: ", message))
			stop("Failed to print results.")
		}) #end try catch for print statement

		it = it+1
		nRej = 0
	}	#replicates (while loop)
}	#end function






#	record scenario type
#        Onset   Dur     Obs
#1       +       +       +       STANDARD
#2       +       -       +
#3       +       -       -       PARADOX
#4       -       -       -       STANDARD
#5       -       +       -
#6       -       +       +       PARADOX
#7       -       -       +       IMPOSSIBLE with sufficiently large sample size
#8       +       +       -       IMPOSSIBLE with sufficiently large sample size
