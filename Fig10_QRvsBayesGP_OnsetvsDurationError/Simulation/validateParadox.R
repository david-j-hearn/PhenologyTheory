source("phenologySimulation.R")
source("phenologyInference.R")
source("processStan.R")
library(quantreg)

run_AnalysisParadox = function(replicates=1000,  outputFile="output.txt", scenarioNumber,append=F, cushionThresh=0.0, seed=12345) {
set.seed(seed)

# set up simulation parameters
minResponse = 0
maxResponse = 365
range = maxResponse - minResponse

#set up sample sizes
minSS = 10
maxSS = 1000
SS = round(runif(replicates,minSS,maxSS))

#year range
minYear = 1850
maxYear = 2025
rangeYear = maxYear - minYear
meanYear = minYear + rangeYear / 2

#onset range
minO = 50 
maxO = 250
rangeO = 30	#max for slopes
H_SD_meanO = 7

#duration range
minD = 10
maxD = 90
rangeD = 20	#max for slopes
H_SD_meanD = 7

#onset slope range
minSlopeO_raw = -rangeO / rangeYear
maxSlopeO_raw = rangeO / rangeYear
H_SD_SlopeO=0.1

#duration slope range
minSlopeD_raw = -rangeD / rangeYear
maxSlopeD_raw = rangeD / rangeYear
H_SD_SlopeD=0.1

#set up sigmas
minSigma = 5
maxSigma = 15
H_SD_Sigma = maxSigma/2
sigma = runif(replicates,minSigma,maxSigma)

#the true cutoff is 2.0, but using cushion gives a bit of a buffer around 0 values - easier to detect effect
cushionOrig=0.2

#how many times that infeasible parameter values are sampled -and rejected- will be tolerated
maxRej = 100

#set up anchor means and slopes
if(scenarioNumber == 1) {
		cushion = cushionOrig
	slopeO = runif(replicates, cushion * maxSlopeO_raw, maxSlopeO_raw)
	slopeD = runif(replicates, cushion * maxSlopeD_raw, maxSlopeD_raw)
	meanO = rep(F, replicates)
	meanD = rep(F, replicates)
	for(i in 1:replicates) {
		cushion = cushionOrig
		nRej = 0
		while(!meanO[i] || !meanD[i]) {
			nRej = nRej + 1
			meanO[i] = setMean(slope=slopeO[i], minYear = minYear, maxYear = maxYear, minR = minO, maxR = maxO)
			meanD[i] = setMean(slope=slopeD[i], minYear = minYear, maxYear = maxYear, minR = minD, maxR = maxD)
			if(nRej>maxRej) {
				print("Failed to obtain means for scenario 1")
				return()
			}
		print(paste(i, ": ", nRej, ": ",  slopeO[i], " ", slopeD[i], " ", meanO[i], " ", meanD[i]))
		}
	}
}
else if(scenarioNumber == 2) {
	slopeO = rep(F, replicates)
	slopeD = rep(F, replicates)
	meanO = rep(F, replicates)
	meanD = rep(F, replicates)

	tminSlopeD = minSlopeD_raw
	if(minSlopeD_raw < (-(2-cushionThresh)*maxSlopeO_raw)) {
		tminSlopeD = -(2-cushionThresh)*maxSlopeO_raw
	}

	for(i in 1:replicates) {
		cushion = cushionOrig
		#check slopes
		nRej = 0
		while(!slopeO[i] || !slopeD[i]) {
			if(tminSlopeD<0.01) { cushion = 0.5 }
			else { cushion = cushionOrig }
			slopeD[i] = runif(1, tminSlopeD, cushion * tminSlopeD)	
			if((-slopeD[i]/(2-cushionThresh)) > maxSlopeO_raw) {
				print(paste(i, ": ", nRej, ": Rejecting ", slopeO[i], " because the maximum possible onset slope is less than required, scenario 2."))
				nRej = nRej +1
				if(nRej>maxRej) {
					print("Failed to obtain slopes scenario 2")
					return()
				}
				slopeD[i] = F
			}
			if(slopeD[i]) {
				tminSlopeO = (-slopeD[i]) / (2-cushionThresh)
				slopeO[i] = runif(1, tminSlopeO, maxSlopeO_raw)
			}
			if(slopeO[i] / (-slopeD[i]/2)<1) {	#shouldn't ever reach here
				print(paste(i, ": ", nRej, ": Rejecting ", slopeO[i], " because an error occurred."))
				nRej = nRej +1
				if(nRej>maxRej) {
					print("Failed to obtain slopes scenario 2")
					return()
				}
				slopeO[i]=F
			}
		}
		#check means
		nRej = 0
		while(!meanO[i] || !meanD[i]) {
			nRej = nRej + 1
			meanO[i] = setMean(slope=slopeO[i], minYear = minYear, maxYear = maxYear, minR = minO, maxR = maxO)
			meanD[i] = setMean(slope=slopeD[i], minYear = minYear, maxYear = maxYear, minR = minD, maxR = maxD)
			if(nRej>maxRej) {
				print("Failed to obtain means for scenario 2")
				return()
			}
		print(paste(i, ": ", nRej, ": ",  slopeO[i], " ", slopeD[i], " >=1: ", -slopeO[i]/(slopeD[i]/2), " ", meanO[i], " ", meanD[i]))
		}
	}
}
else if(scenarioNumber == 3) {
	slopeO = rep(F, replicates)
	slopeD = rep(F, replicates)
	meanO = rep(F, replicates)
	meanD = rep(F, replicates)

	tminSlopeD = minSlopeD_raw
	if(minSlopeD_raw < (-(2-cushionThresh)*maxSlopeO_raw)) {
		tminSlopeD = -(2-cushionThresh)*maxSlopeO_raw
	}

	for(i in 1:replicates) {
		cushion = cushionOrig
		#check slopes
		nRej = 0
		while(!slopeO[i] || !slopeD[i]) {
			if(tminSlopeD<0.01) { cushion = 0.5 }
			else { cushion = cushionOrig }
			slopeD[i] = runif(1, tminSlopeD, cushion * tminSlopeD)	
			if((-slopeD[i]/(2-cushionThresh)) > maxSlopeO_raw) {
				print(paste(i, ": ", nRej, ": Rejecting ", slopeO[i], " because the maximum possible onset slope is less than required, scenario 3."))
				nRej = nRej +1
				if(nRej>maxRej) {
					print("Failed to obtain slopes scenario 3")
					return()
				}
				slopeD[i] = F
			}
			if(slopeD[i]) {
				tmaxSlopeO = -slopeD[i] / (2-cushionThresh)
				if(tmaxSlopeO<0.01) { cushion = 0.5 }
				else { cushion = cushionOrig }
				slopeO[i] = runif(1, cushion*tmaxSlopeO, tmaxSlopeO)
			}
			if(slopeO[i] / (-slopeD[i]/2)>1) {	#shouldn't ever reach here
				print(paste(i, ": ", nRej, ": Rejecting ", slopeO[i], " because an error occurred."))
				nRej = nRej +1
				if(nRej>maxRej) {
					print("Failed to obtain slopes scenario 3")
					return()
				}
				slopeO[i]=F
			}
		}
		#check means
		nRej = 0
		while(!meanO[i] || !meanD[i]) {
			nRej = nRej + 1
			meanO[i] = setMean(slope=slopeO[i], minYear = minYear, maxYear = maxYear, minR = minO, maxR = maxO)
			meanD[i] = setMean(slope=slopeD[i], minYear = minYear, maxYear = maxYear, minR = minD, maxR = maxD)
			if(nRej>maxRej) {
				print("Failed to obtain means for scenario 3")
				return()
			}
		print(paste(i, ": ", nRej, ": ",  slopeO[i], " ", slopeD[i], " <=1: ", -slopeO[i]/(slopeD[i]/2), " ", meanO[i], " ", meanD[i]))
		}
	}
}
else if(scenarioNumber == 4) {
		cushion = cushionOrig
	slopeO = runif(replicates, minSlopeO_raw, cushion * minSlopeO_raw)
	slopeD = runif(replicates, minSlopeD_raw, cushion * minSlopeD_raw)
	meanO = rep(F, replicates)
	meanD = rep(F, replicates)
	for(i in 1:replicates) {
		cushion = cushionOrig
		nRej = 0
		while(!meanO[i] || !meanD[i]) {
			nRej = nRej + 1
			meanO[i] = setMean(slope=slopeO[i], minYear = minYear, maxYear = maxYear, minR = minO, maxR = maxO)
			meanD[i] = setMean(slope=slopeD[i], minYear = minYear, maxYear = maxYear, minR = minD, maxR = maxD)
			if(nRej>maxRej) {
				print("Failed to obtain means for scenario 4")
				return()
			}
		print(paste(i, ": ", nRej, ": ",  slopeO[i], " ", slopeD[i], " ", meanO[i], " ", meanD[i]))
		}
	}
}
else if(scenarioNumber == 5) {
	slopeO = rep(F, replicates)
	slopeD = rep(F, replicates)
	meanO = rep(F, replicates)
	meanD = rep(F, replicates)

	tmaxSlopeD = maxSlopeD_raw
	if((-(2-cushionThresh)*minSlopeO_raw)< maxSlopeD_raw) {
		tmaxSlopeD = (-(2-cushionThresh)*minSlopeO_raw)
	}

	for(i in 1:replicates) {
		cushion = cushionOrig
		#check slopes
		nRej = 0
		while(!slopeO[i] || !slopeD[i]) {
			if(tmaxSlopeD<0.01) { cushion = 0.5 }
			else { cushion = cushionOrig }
			slopeD[i] = runif(1, cushion*tmaxSlopeD, tmaxSlopeD)	
			if(minSlopeO_raw>(-slopeD[i]/2)) {
				print(paste(i, ": ", nRej, ": Rejecting slope duration ", slopeD[i], " because it is smaller than feasible for slope onset for scenario 5"))
				nRej = nRej +1
				if(nRej>maxRej) {
					print("Failed to obtain slopes scenario 5")
					return()
				}
				slopeD[i] = F
			}
			if(slopeD[i]) {
				tmaxSlopeO = (-slopeD[i] / (2+cushionThresh))
				slopeO[i] = runif(1, minSlopeO_raw, tmaxSlopeO)
			}
			if((slopeO[i]) / (-slopeD[i]/2)<1) {	#shouldn't ever reach here
				print(paste(i, ": ", nRej, ": Rejecting ", slopeO[i], " because an error occurred."))
				nRej = nRej +1
				if(nRej>maxRej) {
					print("Failed to obtain slopes scenario 5")
					return()
				}
				slopeO[i]=F
			}
		}
		#check means
		nRej = 0
		while(!meanO[i] || !meanD[i]) {
			nRej = nRej + 1
			meanO[i] = setMean(slope=slopeO[i], minYear = minYear, maxYear = maxYear, minR = minO, maxR = maxO)
			meanD[i] = setMean(slope=slopeD[i], minYear = minYear, maxYear = maxYear, minR = minD, maxR = maxD)
			if(nRej>maxRej) {
				print("Failed to obtain means for scenario 5")
				return()
			}
		print(paste(i, ": ", nRej, ": ",  slopeO[i], " ", slopeD[i], " >=1: ", -slopeO[i]/(slopeD[i]/2), " ", meanO[i], " ", meanD[i]))
		}
	}
}
else if(scenarioNumber == 6) {
	slopeO = rep(F, replicates)
	slopeD = rep(F, replicates)
	meanO = rep(F, replicates)
	meanD = rep(F, replicates)

	tmaxSlopeD = maxSlopeD_raw
	if((-(2-cushionThresh)*minSlopeO_raw)< maxSlopeD_raw) {
		tmaxSlopeD = (-(2-cushionThresh)*minSlopeO_raw)
	}

	for(i in 1:replicates) {
		cushion = cushionOrig
		#check slopes
		nRej = 0
		while(!slopeO[i] || !slopeD[i]) {
			if(tmaxSlopeD<0.01) { cushion = 0.5 }
			else { cushion = cushionOrig }
			slopeD[i] = runif(1, cushion*tmaxSlopeD, tmaxSlopeD)	
			if(minSlopeO_raw>(-slopeD[i]/2)) {
				print(paste(i, ": ", nRej, ": Rejecting slope duration ", slopeD[i], " because it is smaller than feasible for slope onset for scenario 6"))
				nRej = nRej +1
				if(nRej>maxRej) {
					print("Failed to obtain slopes scenario 6")
					return()
				}
				slopeD[i] = F
			}
			if(slopeD[i]) {
				tminSlopeO = (-slopeD[i] / (2+cushionThresh))
				if(tminSlopeO<0.01) { cushion = 0.5 }
				else { cushion = cushionOrig }
				slopeO[i] = runif(1, tminSlopeO, cushion*tminSlopeO)
			}
			if((-slopeO[i]) / (slopeD[i]/2)>1) {	#shouldn't ever reach here
				print(paste(i, ": ", nRej, ": Rejecting ", slopeO[i], " because an error occurred."))
				nRej = nRej +1
				if(nRej>maxRej) {
					print("Failed to obtain slopes scenario 6")
					return()
				}
				slopeO[i]=F
			}
		}
		#check means
		nRej = 0
		while(!meanO[i] || !meanD[i]) {
			nRej = nRej + 1
			meanO[i] = setMean(slope=slopeO[i], minYear = minYear, maxYear = maxYear, minR = minO, maxR = maxO)
			meanD[i] = setMean(slope=slopeD[i], minYear = minYear, maxYear = maxYear, minR = minD, maxR = maxD)
			if(nRej>maxRej) {
				print("Failed to obtain means for scenario 6")
				return()
			}
		print(paste(i, ": ", nRej, ": ",  slopeO[i], " ", slopeD[i], " <=1: ", -slopeO[i]/(slopeD[i]/2), " ", meanO[i], " ", meanD[i]))
		}
	}
	}
	else {
		print(paste("Scenario number ", scenarioNumber, " must be between 1 and 6. Quitting."))
		return()
	}

print(mean(slopeO))

#stop("TESTING")

#so far we have set sample sizes, sigmas, slopes, and means for all simulation replicates for the given input scenario
#go through each set of parameters, simulate the data, extract estimates, print results

cat(sprintf("Replicate\tSampleSize\tScenarioNumber\tTrue_Slope_meanOnset\tTrue_Intercept_meanOnset\tTrue_Anchor_meanOnset\tTrue_Slope_meanDuration\tTrue_Intercept_meanDuration\tTrue_Anchor_meanDuration\tTrue_Sigma\tTheor_Slope_meanObserved\tTheor_Intercept_meanObserved\tTheor_Slope_meanCessation\tTheor_Intercept_meanCessation\tTrueObs_meanOnset\tTrueObs_meanDuration\tTheor_meanOnset\tTheor_meanDuration\tHyper_Mean_Slope_meanOnset\tHyper_SD_Slope_meanOnset\tHyper_Mean_Slope_meanDuration\tHyper_SD_Slope_meanDuration\tHyper_Mean_Anchor_meanOnset\tHyper_SD_Anchor_meanOnset\tHyper_Mean_Anchor_meanDuration\tHyper_SD_Anchor_meanDuration\tHyper_Mean_Sigma\tHyper_SD_Sigma\tEst_SR_Slope_meanObserved\tEst_SR_Intercept_meanObserved\tEst_QR_Slope_meanQ10\tEst_QR_Intercept_meanQ10\tEst_QR_Slope_meanQ50\tEst_QR_Intercept_meanQ50\tEst_QR_Slope_meanQ90\tEst_QR_Intercept_meanQ90\tEst_Stan_Slope_meanOnset\tEst_Stan_Slope_meanOnset_q2.5\tEst_Stan_Slope_meanOnset_q97.5\tEst_Stan_Intercept_meanOnset\tEst_Stan_Intercept_meanOnset_q2.5\tEst_Stan_Intercept_meanOnset_q97.5\tEst_Stan_Anchor_meanOnset\tEst_Stan_Anchor_meanOnset_q2.5\tEst_Stan_Anchor_meanOnset_q97.5\tEst_Stan_Slope_meanDuration\tEst_Stan_Slope_meanDuration_q2.5\tEst_Stan_Slope_meanDuration_q97.5\tEst_Stan_Intercept_meanDuration\tEst_Stan_Intercept_meanDuration_q2.5\tEst_Stan_Intercept_meanDuration_q97.5\tEst_Stan_Anchor_meanDuration\tEst_Stan_Anchor_meanDuration_q2.5\tEst_Stan_Anchor_meanDuration_q97.5\tEst_Stan_Sigma\tEst_Stan_Sigma_q2.5\tEst_Stan_Sigma_q97.5\tNumber_Divergences\n"),file=outputFile,append=append)

#set up vectors to store intercepts for output
interceptO = rep(0.0, replicates)
interceptD = rep(0.0, replicates)

print("Done setting up parameters. Starting simulations...")
for(i in 1:replicates) {

print(paste("Scenario ", scenarioNumber, ", replicate ", i))
interceptO[i] =  meanO[i] - meanYear * slopeO[i]
interceptD[i] = meanD[i] - meanYear * slopeD[i]

sim = simulate_observed_wSameCovariates_GP(min=minResponse, max=maxResponse, replicates=SS[i], covariateMin=c(minYear), covariateMax=c(maxYear), alphaOnset=interceptO[i],betaOnset=c(slopeO[i]),alphaDuration=interceptD[i], betaDuration=c(slopeD[i]),sigma=sigma[i])

text = paste("Rep: ", i, ": Scenario ", scenarioNumber, "\nSlope Onset: ", slopeO[i], "\nSlope Duration: ", slopeD[i], "\nMean Duration: ", meanD[i])

plot(sim$onsetDesign$O1, sim$observed, ylim=c(0,365),col="purple", pch=16)
points(sim$durationDesign$D1, sim$t_start, col="blue")
points(sim$durationDesign$D1, sim$t_end, col="red")
segments(sim$durationDesign$D1, sim$t_start, sim$durationDesign$D1, sim$t_end)
text(1950, 300, text, pos=1)


#hyperparameters set with random, small bias, but large SD, so relatively flat
meanHO = slopeO[i] + rnorm(1, 0, abs(slopeO[i])/5)
hyperSlopeO = data.frame(meanHO,H_SD_SlopeO)
meanAnchorO = meanO[i]+rnorm(1,0,10)
hyperAnchorO = c(meanAnchorO,H_SD_meanO)

meanHD = slopeD[i] + rnorm(1, 0, abs(slopeD[i])/5)
hyperSlopeD = data.frame(meanHD,H_SD_SlopeD)
meanAnchorD = meanD[i]+rnorm(1,0,10)
hyperAnchorD = c(meanAnchorD,H_SD_meanD)

meanSigma = abs(sigma[i]+rnorm(1,0,5))
hyperSigma = c(meanSigma, H_SD_Sigma)

#run analysis with stan

resGP = tryCatch({
	runCovariateGPPhenology(response=sim$observed, minResponse=minResponse, maxResponse=maxResponse, onsetCovariates=sim$onsetDesign, durationCovariates=sim$onsetDesign, onsetHyperBeta=hyperSlopeO, onsetHyperAnchor=hyperAnchorO, durationHyperBeta=hyperSlopeD, durationHyperAnchor=hyperAnchorD, sigmaHyper=hyperSigma, dataProvided=T)
	}, error = function(e) {
                message <- conditionMessage(e)
	print(paste("ERROR: ", message))
	list(error=T, 
		error_m="Stan failed to run.")
	
	})
if(resGP$error) { 
	#stop("Stan error")
	next
	}

origVars_Stan = extractVariables(stanRunResult=resGP$result, meanCovariates=meanYear, minCovariates=minYear, maxCovariates=maxYear, responseMin=minResponse, responseMax=maxResponse)

print("Hyperparameters")
print(hyperSlopeO)
print(hyperSlopeD)
print(hyperAnchorO)
print(hyperAnchorD)
print(hyperSigma)

print("multivariate method")
print("true")
print("Anchor onset")
print(origVars_Stan$anchorOnset_q2.5)
print(origVars_Stan$anchorOnset)
print(origVars_Stan$anchorOnset_q97.5)
print(meanO[i])
print("Anchor duration")
print(origVars_Stan$anchorDuration_q2.5)
print(origVars_Stan$anchorDuration)
print(origVars_Stan$anchorDuration_q97.5)
print(meanD[i])
print("Slope onset")
print(origVars_Stan$betasO_q2.5)
print(origVars_Stan$betasO)
print(origVars_Stan$betasO_q97.5)
print(slopeO[i])
print("Slope duration")
print(origVars_Stan$betasD_q2.5)
print(origVars_Stan$betasD)
print(origVars_Stan$betasD_q97.5)
print(slopeD[i])
print("Intercept onset")
print(origVars_Stan$alphaO_q2.5)
print(origVars_Stan$alphaO)
print(origVars_Stan$alphaO_q97.5)
print(as.numeric(interceptO[i]))
print("Intercept duration")
print(origVars_Stan$alphaD_q2.5)
print(origVars_Stan$alphaD)
print(origVars_Stan$alphaD_q97.5)
print(as.numeric(interceptD[i]))
print("Sigma")
print(origVars_Stan$sigma_q2.5)
print(origVars_Stan$sigma)
print(origVars_Stan$sigma_q97.5)
print(sigma[i])

nDiv = sum(resGP$result$diagnostic_summary()$num_divergent)


y = sim$observed
x = sim$onsetDesign$O1

#run quantile regression
resQ10 = rq(y ~ x, tau=0.1)
resQ50 = rq(y ~ x, tau=0.5) 	#could be done together!
resQ90 = rq(y ~ x, tau=0.9) 	#could be done together!
#slopeO_q10 = resQ10$coefficients[names(resQ10$coefficients)=="x"]
#slopeO_q50 = resQ50$coefficients[names(resQ50$coefficients)=="x"]
#slopeO_q90 = resQ90$coefficients[names(resQ90$coefficients)=="x"]

#standard regression
resSR = lm(y ~ x)
slopeO_SR = resSR$coefficients[names(resSR$coefficients)=="x"]

pRes = tryCatch({
	cat(sprintf("%d\t%d\t%d\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%d\n",
#replicate number
	i,
#sample size
	SS[i],
#scenario number
	scenarioNumber,
#true beta_o value
	slopeO[i],
#true alpha_o value
	interceptO[i],
#true mean_o value
	meanO[i],
#true beta_d value
	slopeD[i],
#true alpha_d value
	interceptD[i],
#true mean_d value
	meanD[i],
#True_Sigma,
	sigma[i],
#theoretical beta_observed	(in theory, this is beta_o + beta_d/2)
	slopeO[i] + slopeD[i]/2,
#theoretical alpha_observed	(in theory, this is alpha_o + alpha_d/2)
	interceptO[i] + interceptD[i]/2,
#theoretical beta_cessation
	slopeO[i] + slopeD[i],
#theoretical alpha_cessation
	interceptO[i] + interceptD[i],
#true observed mean onset over the time interval (mean(sim$t_start))
	mean(sim$t_start),
#true observed mean duration over the time interval (mean(sim$t_end - sim$t_start))
	mean(sim$durations),
#theor mean onset over the time interval alpha_o + beta_o * (x_min + x_max) /2 
	interceptO[i] + minYear*slopeO[i] + rangeYear*slopeO[i]/2,
#theor mean duration over the time interval alpha_d + beta_d * (x_min + x_max) /2
	interceptD[i] + minYear*slopeD[i] + rangeYear*slopeD[i]/2,
#hyperparameters (mean, sd) for beta_o bias mean
	meanHO,
#hyperparameters (mean, sd) for beta_o bias SD
	H_SD_SlopeO,
#hyperparameters (mean, sd) for beta_d bias mean
	meanHD,
#hyperparameters (mean, sd) for beta_d bias SD
	H_SD_SlopeD,
#hyperparameters (mean, sd) for mean_o bias mean
	meanAnchorO,
#hyperparameters (mean, sd) for mean_o bias SD
	H_SD_meanO,
#hyperparameters (mean, sd) for mean_d bias mean
	meanAnchorD,
#hyperparameters (mean, sd) for mean_d bias mean
	H_SD_meanD,
#hyperparameters (mean, sd) for sigma bias mean
	meanSigma,
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
	origVars_Stan$betasO[1],
#inferred beta_o q2.5 GP
	origVars_Stan$betasO_q2.5[1],
#inferred beta_o q97.5 GP
	origVars_Stan$betasO_q97.5[1],
#inferred alpha_o GP
	origVars_Stan$alphaO,
#inferred alpha_o q2.5 GP
	origVars_Stan$alphaO_q2.5,
#inferred alpha_o q97.5 GP
	origVars_Stan$alphaO_q97.5,
#inferred mean_o GP
	origVars_Stan$anchorOnset,
#inferred mean_o q2.5 GP
	origVars_Stan$anchorOnset_q2.5,
#inferred mean_o q97.5 GP
	origVars_Stan$anchorOnset_q97.5,
#inferred beta_d GP
	origVars_Stan$betasD[1],
#inferred beta_d q2.5 GP
	origVars_Stan$betasD_q2.5[1],
#inferred beta_d q97.5 GP
	origVars_Stan$betasD_q97.5[1],
#inferred alpha_d GP
	origVars_Stan$alphaD,
#inferred alpha_d q2.5 GP
	origVars_Stan$alphaD_q2.5,
#inferred alpha_d q97.5 GP
	origVars_Stan$alphaD_q97.5,
#inferred mean_d GP
	origVars_Stan$anchorDuration,
#inferred mean_d q2.5 GP
	origVars_Stan$anchorDuration_q2.5,
#inferred mean_d q97.5 GP
	origVars_Stan$anchorDuration_q97.5,
#inferred sigma GP,
        origVars_Stan$sigma,
#inferred sigma q2.5 GP,
        origVars_Stan$sigma_q2.5,
#inferred sigma q97.5 GP,
        origVars_Stan$sigma_q97.5,
#number of divergences
	nDiv
	),file=outputFile,append=T)
}, error = function(e) {
                message <- conditionMessage(e)
                print(paste("Print statement failed. Retrying. Error: ", message))
        }) #end try catch for print statement

}	#for loop for simulations and analyses

print(paste("Finished analysis. Results are in the file ", outputFile))

}	#end analysis function

setMean = function(slope, minYear, maxYear, minR, maxR) {
	range = maxYear - minYear
	if(slope>0) { 		#+slope
		if(slope*range + minR > maxR) {
			return(F)
		}
	sampleRange = maxR - (minR + range*slope)
	realR = minR + runif(1,0,sampleRange)
	mean = realR + slope*range/2
	return(mean)
	}
	else { 			#-slope
		if((maxR + range*slope) < minR) {
			return(F)
		}
	sampleRange = (maxR + range*slope) - minR
	realR = maxR - runif(1,0,sampleRange)
	mean = realR + slope*range/2
	return(mean)
	}
}

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
