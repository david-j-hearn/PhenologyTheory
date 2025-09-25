
source("phenologyAnalysisTools.R")
source("phenologyInference.R")
source("phenologySimulation.R")
library(phest) #from: https://github.com/willpearse/phest

#This analysis explores how well methods reconstruct the mean/sd onset, duration, cessation, and peak phenophase based on simulated data.
run_Analysis1 = function(outputFile = "output.txt", nrep = 1000, seed=12345, append=F) {
set.seed(seed)


#set up simulation parameters at beta scale
#	Sample size 
minSS =	5 
maxSS = 1000

#	Pearse comparison
#pearsePercent = 0.5
maxPK = 50
minPK = minSS
pearseInc=5
SS = seq(minPK,maxPK,pearseInc)

#beta onset, beta duration simulations or GP simulations
GPPercent = 0.5

#how frequent to run MLE
MLEPercent = 0.25

#beta shape parameter bounds
minS = 1.5
maxS = 3000

#range of day of year values
min = 0
max = 365
d = max-min

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

maxBias_mO = min_mO / d 	#must be less than or equal to min_mO
maxBias_SDO = min_SDO / d
maxBias_mD = min_mD / d
maxBias_SDD = min_SDD / d

#hyperparameter accuracy
perfectHyperPercent = 0.1

#beta scale for initial value sample parameters
init_min_mO = 0.2 
init_max_mO = 0.8
init_min_SDO = 0.05
init_max_SDO = 0.15

init_min_mD = 0.1
init_max_mD = 0.5
init_min_SDD = 0.05
init_max_SDD = 0.15

#surrogate variable name for number of replicates
replicates = nrep

cat(sprintf("AP_Replicate(seed: %d)\tAP_SampleSize\tAP_SimulationModel\tAP_PearseSampleSize\tAP_PearseK\tInit_MeanOnset\tInit_SDOnset\tInit_MeanDuration\tInit_SDDuration\tTrueParam_MeanOnset\tTrueParam_SDOnset\tTrueParam_MeanDuration_prescaled_BB\tTrueParam_MeanDuration\tTrueParam_SDDuration\tTrueParam_MeanCessation\tTrueParam_SDCessation\tTrueObserved_FirstOnset\tTheorParam_PeakPhenophase_fromTrueModel\tTheorParam_ExpectedFirstOnset_fromTrueModel\tTheorParam_q2.5FirstOnset_fromTrueModel\tTheorParam_q97.5FirstOnset_fromTrueModel\tTheorParam_PercentOverlapOnsetCessation_fromTrueModel\tTheorParam_CohensD_fromTrueModel\tTrueHyper_mean_MeanOnset\tTrueHyper_sd_MeanOnset\tTrueHyper_mean_SDOnset\tTrueHyper_sd_SDOnset\tTrueHyper_mean_MeanDuration_prescaled_BB\tTrueHyper_mean_MeanDuration\tTrueHyper_sd_MeanDuration\tTrueHyper_mean_SDDuration\tTrueHyper_sd_SDDuration\tEst_MAP_MeanOnset\tEst_MAP_SDOnset\tEst_MAP_MeanDuration\tEst_MAP_SDDuration\tEstTheor_MAP_PeakPhenophase\tEstTheor_MAP_FirstOnset\tEstTheor_MAP_q2.5FirstOnset\tEstTheor_MAP_q97.5FirstOnset\tEst_MLE_MeanOnset\tEst_MLE_SDOnset\tEst_MLE_MeanDuration\tEst_MLE_SDDuration\tEstTheor_MLE_PeakPhenophase\tEstTheor_MLE_FirstOnset\tEstTheor_MLE_q2.5FirstOnset\tEstTheor_MLE_q97.5FirstOnset\tEst_Stan_HMC_MeanOnset\tEst_Stan_HMC_MeanDuration\tEst_Stan_HMC_MeanCessation\tEst_Stan_HMC_Sigma\tEst_Stan_HMC_q2.5_MeanOnset\tEst_Stan_HMC_q2.5_MeanDuration\tEst_Stan_HMC_q2.5_MeanCessation\tEst_Stan_HMC_q2.5_Sigma\tEst_Stan_HMC_q97.5_MeanOnset\tEst_Stan_HMC_q97.5_MeanDuration\tEst_Stan_HMC_q97.5_MeanCessation\tEst_Stan_HMC_q97.5_Sigma\tEstTheor_Stan_HMC_FirstOnset\tEstTheor_Stan_HMC_q2.5_FirstOnset\tEstTheor_Stan_HMC_q97.5_FirstOnset\tEstTheor_Stan_HMC_PeakPhenophase\tEst_Stan_MAP_MeanOnset\tEst_Stan_MAP_MeanDuration\tEst_Stan_MAP_MeanCessation\tEst_Stan_MAP_Sigma\tEstTheor_Stan_MAP_FirstOnset\tEstTheor_Stan_MAP_q2.5_FirstOnset\tEstTheor_Stan_MAP_q97.5_FirstOnset\tEstTheor_Stan_MAP_PeakPhenophase\tEst_Quantile_q10\tEst_Quantile_q50\tEst_Quantile_q90\tEst_Pearse_FullSample_FirstOnset\tEst_Pearse_FullSample_q2.5_FirstOnset\tEst_Pearse_FullSample_q97.5_FirstOnset\tEst_Pearse_SubSample_FirstOnset\tEst_Pearse_SubSample_q2.5_FirstOnset\tEst_Pearse_SubSample_q97.5_FirstOnset\n",seed),file=outputFile,append=append)

cur = 1
while(cur <= replicates) {
	print(paste(cur, ": Attempting replicate"))

	#true parameter values scaled between 0 and 1 ("beta scale")
	true_mO = ( runif(1,min_mO,max_mO) - min ) / d
	true_SDO = runif(1,min_SDO, max_SDO) / d
	true_mD = runif(1,min_mD, max_mD) / d
	true_SDD = runif(1,min_SDD, max_SDD) / d
	true_mD_scaled = true_mD * (1 - true_mO)
	true_mC = true_mO + true_mD_scaled

	#test parameter feasibility - Should alway be feasible here!
	if(true_mO+true_mD*(1-true_mO) > 0.9999 ) {
		print(paste("True means and SDs outside of feasible range at replicate ", cur))
		#print(paste("tmo: ", min + true_mO * d, " tsdo: ", true_SDO * d, " tmd: ", true_mD * d, " tsdd: ", true_SDD * d))
		#print(paste("tmo: ", true_mO, " tsdo: ", true_SDO, " tmd: ", true_mD, " tsdd: ", true_SDD))
		next
	}

	#pick sample size and Pearse k size
	n = round(runif(1, minSS, maxSS))
	pn = sample(SS,1)
	if(pn > n) {
		pn = floor(n / pearseInc) * pearseInc #get the nearest lower or equal value that is a multiple of pearseInc
	}
	pk=pn

	print(paste(cur, ": Got sample sizes: N=", n, " Pearse subsample=", pn))

	#simulate data with true parameter values at original scale
	print(paste(cur, ": simulating data"))

	#pick the simulation type and obtain type-specific theoretical parameter values
	if(runif(1) <= GPPercent) {
		print(paste(cur, ": the simulation type is Gaussian Process (GP)"))
		type = "GP"
		sim = simulate_observed_GP_M2(n=n, min=min, max=max, mu_O = true_mO * d + min, mu_C = (true_mO + true_mD * (1 - true_mO))*d + min, sd = true_SDO * d)
		if(sim$error) {
		print(paste(cur, ": failed to simulate data under GP. Trying again."))
		next
		}
	#get the proportion overlap between the true onset and true cessation distributions
		overlap = tryCatch( {
			getOverlap_GP(sim$onset_mean, sim$cessation_mean, sim$sigma)
		}, error = function(e) {
			F
		})

		if(!overlap) {
			print(paste(cur, ": failed to get overlap."))
			next
		}
		else {
		print(paste(cur, ": got overlap under GP model: ", overlap))
		}
	}
	else {
		print(paste(cur, ": the simulation type is beta onset and beta duration (BB)"))
		type = "BB"
		sim = simulate_observed(n, min=min, max=max, true_mO * d + min, true_SDO * d, true_mD * d, true_SDD * d)
		if(sim$error) {
		print(paste(cur, ": failed to simulate data under beta onset, beta duration model. Trying again."))
		next
		}
	#get the proportion overlap between the true onset and true cessation distributions
		overlap = tryCatch( {
			getOverlap(sim$alpha_s, sim$beta_s, sim$alpha_d, sim$beta_d)
		}, error = function(e) {
			F
		})

		if(!overlap) {
			print(paste(cur, ": failed to get overlap."))
			next
		}
		else {
			print(paste(cur, ": got overlap under BB model: ", overlap))
		}
	}
	if(sim$error) {
		print(paste("Simulation failed at replicate ", cur, ". Trying again."))
		next
	}


	cd = getCohensD(true_mC, true_mO, true_SDO, sim$cessation_sd)
	print(paste(cur, ": Got Cohen's d for the onset and cessation: ", cd))


	pearse_full = tryCatch( {
		weib.limit(x=sim$observed, k=pk, upper=F, alpha=0.05)
	}, error = function(e) {
		c(F)
	})

	if(!pearse_full[1]) {
		pfe_f = NA
		plci_f = NA
		puci_f = NA
	}
	else {
		pfe_f = pearse_full[1]
		plci_f = pearse_full[2]
		puci_f = pearse_full[3]
		print(paste(cur, ": Got earliest extremes using Pearse et al (2017) methodology and full data: ", pfe_f, "(", plci_f, ",", puci_f, ")"))
	}

	pearse_sub = tryCatch( {
		weib.limit(x=sample(sim$observed,pn,replace=F), k=pk, upper=F, alpha=0.05)
	}, error = function(e) {
		c(F)
	})

	if(!pearse_sub[1]) {
		pfe_s = NA
		plci_s = NA
		puci_s = NA
	}
	else {
		pfe_s = pearse_sub[1]
		plci_s = pearse_sub[2]
		puci_s = pearse_sub[3]
		print(paste(cur, ": Got earliest extremes using Pearse et al (2017) methodology and subsampled data: ", pfe_s, "(", plci_s, ",", puci_s, ")"))
	}

	#get the quantile data
	q = quantile(sim$observed, probs = c(0.10, 0.50, 0.90))
	print(paste(cur, ": Got quantiles using Park et al. (2024) criteria where onset is 10%, peak 50% and cessation 90% : ", q[1], ", ", q[2], ", ", q[3]))

	#set up the initial parameter values to seed the optimization process at original scale
	init_mO = runif(1, init_min_mO, init_max_mO) * d + min
	init_SDO = runif(1, init_min_SDO, init_max_SDO) * d 
	init_mD = runif(1, init_min_mD, init_max_mD) * d 
	init_SDD = runif(1, init_min_SDD, init_max_SDD) * d 

	init_params = c(init_mO, init_SDO, init_mD, init_SDD)
	init_params_beta = init_params
	init_params_beta[1] =  beta_alpha((init_params[1] - min)/d , init_params[2]/d)
	init_params_beta[2] =  beta_beta((init_params[1] - min)/d , init_params[2]/d)
	init_params_beta[3] =  beta_alpha(init_params[3]/d, init_params[4]/d)
	init_params_beta[4] =  beta_beta(init_params[3]/d, init_params[4]/d)

	if(!isBetaFeasible(init_params_beta[1],init_params_beta[2], minS, maxS) || !isBetaFeasible(init_params_beta[3],init_params_beta[4], minS, maxS)) {
		print("Infeasible initial parameter values.")
		#print(paste("imo: ", init_mO, " isdo: ", init_SDO, " imd: ", init_mD, " isdd: ", init_SDD))
		print(init_params_beta)
		next
	}


	if(runif(1) < perfectHyperPercent) {	#set high precision, unbiased priors for a small percentage of replicates to show that method works with good priors
		print(paste(cur, ": Setting unbiased, high-precision priors."))
		hSigma_mO = maxPrecision_mO * d
		hSigma_SDO = maxPrecision_SDO * d
		hSigma_mD = maxPrecision_mD * d
		hSigma_SDD = maxPrecision_SDD * d
		
		hMean_mO = min + true_mO * d
		hMean_SDO = true_SDO * d
		hMean_mD = true_mD * d
		hMean_mD_scaled = true_mD_scaled * d
		hMean_SDD = true_SDD * d
	}
	else {		#set priors with varying strength of bias and precision (expected bias is 0, and mean precision is minPrecision + ( minPrecision - maxPrecision ) /2)

		hSigma_mO = runif(1,maxPrecision_mO, minPrecision_mO) * d	#~reciprocal precision, really...
		hSigma_SDO = runif(1,maxPrecision_SDO, minPrecision_SDO) * d
		hSigma_mD = runif(1,maxPrecision_mD, minPrecision_mD) * d
		hSigma_SDD = runif(1,maxPrecision_SDD, minPrecision_SDD) * d
		
		hMean_mO = runif(1,true_mO - maxBias_mO, true_mO + maxBias_mO) * d + min
		hMean_SDO = runif(1,true_SDO - maxBias_SDO, true_SDO + maxBias_SDO) * d
		hMean_mD = runif(1,true_mD - maxBias_mD, true_mD + maxBias_mD) * d 
		hMean_mD_scaled = runif(1,true_mD*(1-true_mO) - maxBias_mD, true_mD*(1-true_mO) + maxBias_mD) * d 
		hMean_SDD = runif(1,true_SDD - maxBias_SDD, true_SDD + maxBias_SDD) * d 
	}

	beta_hyperparameters = rep(x=0,times=8)
	beta_hyperparameters[1] = beta_alpha((hMean_mO-min)/d, hSigma_mO / d)
	beta_hyperparameters[2] = beta_beta((hMean_mO-min)/d, hSigma_mO / d)
	beta_hyperparameters[3] = beta_alpha(hMean_mD / d, hSigma_mD / d)
	beta_hyperparameters[4] = beta_beta(hMean_mD / d, hSigma_mD / d)
	beta_hyperparameters[5] = beta_alpha(hMean_SDO / d, hSigma_SDO / d)
	beta_hyperparameters[6] = beta_beta(hMean_SDO / d, hSigma_SDO / d)
	beta_hyperparameters[7] = beta_alpha(hMean_SDD / d, hSigma_SDD / d)
	beta_hyperparameters[8] = beta_beta(hMean_SDD / d, hSigma_SDD / d)
	
	#check that the hyperparameters are feasible (i.e., between minShape and maxShape, as set above)
	if(!isBetaFeasible(beta_hyperparameters[1],beta_hyperparameters[2],minS,maxS)  ||
	!isBetaFeasible(beta_hyperparameters[3],beta_hyperparameters[4],minS,maxS)  ||
	!isBetaFeasible(beta_hyperparameters[5],beta_hyperparameters[6],minS,maxS)  ||
	!isBetaFeasible(beta_hyperparameters[7],beta_hyperparameters[8],minS,maxS)  ) {

		print(paste("Infeasible beta hyperparameters at replicate ", cur))
		print(paste("hm_mo: ", hMean_mO, " hsd_mo: ", hSigma_mO, " hm_sdo: ", hMean_SDO, " hsd_sdo: ", hSigma_SDO))
		print(paste("hm_md: ", hMean_mD, " hsd_md: ", hSigma_mD, " hm_sdd: ", hMean_SDD, " hsd_sdd: ", hSigma_SDD))
		print(beta_hyperparameters)
		next
	}
	hyperparameters = c(hMean_mO,hMean_SDO,hMean_mD,hMean_SDD,hSigma_mO,hSigma_SDO,hSigma_mD,hSigma_SDD)
	hyperparameters_Stan = c(hMean_mO,hSigma_mO,hMean_mD_scaled,hSigma_mD,hMean_SDO,hSigma_SDO)


	print(paste(cur, ": Running optimization: Stan"))
	resultStan = tryCatch({
		runUnivariateGPPhenology(sim$observed, min=min, max=max, hyperparameters = hyperparameters_Stan, dataProvided=T) 
	}, error = function(e) {
		list(error=T)
	})
	if(resultStan$error) {
		print(paste("Stan sampling failed at replicate ", cur))
		next
	}

	runMLE = (runif(1) < MLEPercent)
	if(runMLE) { 
		print(paste(cur, ": Running optimization: MLE")) 
		resultMLE = tryCatch({ 
			runMLEPhenology(sim$observed, min=min, max=max, init_params = init_params, dataProvided=T, minS=minS, maxS=maxS) 
		}, error = function(e) { 
			list(error=T) 
		}) 
		if(resultMLE$error) { 
			print(paste("MLE Optimization failed at replicate ", cur)) 
			next 
		}
	}
	else {
		print(paste(cur, ": Skipping optimization: MLE")) 

	}

	print(paste(cur, ": Running optimization: MAP"))
	resultMAP = tryCatch({
		runMAPPhenology(sim$observed, min=min, max=max, init_params = init_params, hyperparameters = hyperparameters, dataProvided=T, minS=minS, maxS=maxS) 
	}, error = function(e) {
		list(error=T)
	})
	if(resultMAP$error) {
		print(paste("MAP Optimization failed at replicate ", cur))
		next
	}

	print(paste(cur, ": Done optimizations!"))

	if(!runMLE) { #Skipped MLE

		est_mO_MLE = NA
		est_SDO_MLE = NA
		est_mD_MLE = NA
		est_SDD_MLE = NA
		est_peak_MLE = NA
		MLEEarliestEst = c(NA, NA, NA)
	}
	else {	#Ran MLE
		#	MLE parameter estimates
		par = resultMLE$par
		est_mO_MLE = min + beta_mean(par[1],par[2]) * d
		est_SDO_MLE = beta_sd(par[1],par[2]) * d
		est_mD_MLE = beta_mean(par[3],par[4]) * d
		est_SDD_MLE = beta_sd(par[3],par[4]) * d
	
		print(paste(cur, ": getting peak phenophase for MLE results"))
		est_peak_MLE = tryCatch({ 
			min + getPeakPhenophase((est_mO_MLE-min)/d, est_SDO_MLE/d, est_mD_MLE/d, est_SDD_MLE/d) * d
		}, error = function(e) {
			F
		})
	
		if(!est_peak_MLE) {
			est_peak_MLE = -1.0
			print(paste(cur, ": Failed to get peak phenophase for MLE"))
		}
		else {
			print(paste(cur, ": Got peak phenophase for MLE results: ", est_peak_MLE))
		}
	
		print(paste(cur, ": Estimating earliest flowering time for MLE results"))
		MLEEarliestEst = tryCatch( {
			E_first_start_time(n,par[1],par[2])
		}, error = function(e) {
			c(F)
		})
		if(!MLEEarliestEst[1]) {
			MLEEarliestEst = c(NA,NA,NA)
		}
		else {
			MLEEarliestEst = min + MLEEarliestEst * d
		}
	}

	#	MAP estimates
	par = resultMAP$par
	est_mO_MAP = min + beta_mean(par[1],par[2]) * d
	est_SDO_MAP = beta_sd(par[1],par[2]) * d
	est_mD_MAP = beta_mean(par[3],par[4]) * d
	est_SDD_MAP = beta_sd(par[3],par[4]) * d

	est_peak_MAP = tryCatch( {
		min + getPeakPhenophase((est_mO_MAP-min)/d, est_SDO_MAP/d, est_mD_MAP/d, est_SDD_MAP/d) * d
	}, error = function(e) {
		F
	})

	if(!est_peak_MAP) {
		est_peak_MAP = -1.0
		print(paste(cur, ": Failed to get peak phenophase for MAP"))
	}
	else {
		print(paste(cur, ": Got peak phenophase for MAP results: ", est_peak_MAP))
	}

	print(paste(cur, ": Estimating earliest flowering time for MAP results"))
	MAPEarliestEst = tryCatch( {
		E_first_start_time(n,par[1],par[2])
	}, error = function(e) {
		c(F)
	})
	if(!MAPEarliestEst[1]) {
		MAPEarliestEst = c(NA,NA,NA)
	}
	else {
		MAPEarliestEst = min + MAPEarliestEst * d
	}
	
	#	Stan estimates
	par_orig_scale = resultStan$par_orig_scale
	par_orig_scale_q2.5 = resultStan$par_orig_scale_q2.5
	par_orig_scale_q97.5 = resultStan$par_orig_scale_q97.5
	est_mO_Stan = par_orig_scale[1] 
	est_mD_Stan = par_orig_scale[2] 
	est_mC_Stan = par_orig_scale[3] 
	est_SD_Stan = par_orig_scale[4] 
	est_mO_Stan_q2.5 = par_orig_scale_q2.5[1] 
	est_mD_Stan_q2.5 = par_orig_scale_q2.5[2] 
	est_mC_Stan_q2.5 = par_orig_scale_q2.5[3] 
	est_SD_Stan_q2.5 = par_orig_scale_q2.5[4] 
	est_mO_Stan_q97.5 = par_orig_scale_q97.5[1] 
	est_mD_Stan_q97.5 = par_orig_scale_q97.5[2] 
	est_mC_Stan_q97.5 = par_orig_scale_q97.5[3] 
	est_SD_Stan_q97.5 = par_orig_scale_q97.5[4] 

	est_peak_Stan = getPeakPhenophase_GP(mu_o = est_mO_Stan , mu_c = est_mC_Stan)
	print(paste(cur, ": Got peak phenophase for stan results: ", est_peak_Stan))

	print(paste(cur, ": Estimating earliest flowering time for stan results"))
	StanEarliestEst = tryCatch( {
		E_first_start_time_GP(N=n,mu_O=est_mO_Stan,sigma=est_SD_Stan, min=min,max=max)
	}, error = function(e) {
		c(NA,NA,NA)
	})
	
	par_orig_scale_MAP = resultStan$par_orig_scale_MAP
	est_mO_Stan_MAP = par_orig_scale_MAP[1] 
	est_mD_Stan_MAP = par_orig_scale_MAP[2] 
	est_mC_Stan_MAP = par_orig_scale_MAP[3] 
	est_SD_Stan_MAP = par_orig_scale_MAP[4] 

	est_peak_Stan_MAP = getPeakPhenophase_GP(mu_o = est_mO_Stan_MAP , mu_c = est_mC_Stan_MAP)
	print(paste(cur, ": Got peak phenophase for stan MAP results: ", est_peak_Stan_MAP))

	print(paste(cur, ": Estimating earliest flowering time for stan MAP results"))
	StanEarliestEst_MAP = tryCatch( {
		E_first_start_time_GP(N=n,mu_O=est_mO_Stan_MAP,sigma=est_SD_Stan_MAP, min=min,max=max)
	}, error = function(e) {
		c(NA,NA,NA)
	})

	if(type=="GP") {
		#get the true peak of the phenophase at original scale
		truePeak = min + getPeakPhenophase_GP(true_mO, true_mC) * d
		print(paste(cur, ": Got true peak: ", truePeak))
		trueEarliest = min(sim$t_start)
		e_first_start_time = sim$e_first_start_time
		true_mO = min + true_mO * d
		true_SDO = true_SDO * d
		true_mD = true_mD * d
		true_SDD = 0 
		true_mC = min + true_mC * d
		true_SDC = true_SDO 		#same SD for onset and cessation
	}
	else {
		#get the true peak of the phenophase at original scale
		truePeak = tryCatch({
			min + getPeakPhenophase(true_mO, true_SDO, true_mD, true_SDD) * d
		}, error = function(e) {
			F
		})
		if(truePeak) {
			print(paste(cur, ": Got true peak: ", truePeak))
		}
		else {
			truePeak = -1.0
			print("Failed to get true peak")
		}

		trueEarliest = min(sim$t_start)
		e_first_start_time = sim$e_first_start_time
		true_mO = min + true_mO * d
		true_SDO = true_SDO * d
		true_mD = true_mD * d
		true_SDD = true_SDD * d
		true_mC = min + true_mC * d
		true_SDC = sim$cessation_sd
	}
	true_mD_scaled = true_mD_scaled*d

	print(paste("Replicate ", cur, " stats: simulation type: ", type, " N: ", n, " k: ", pn, " true_mO: ", true_mO, " true_mD (scaled): ", true_mD, " (", true_mD_scaled, ") true_mC: ", true_mC))
	print(paste(cur, ": printing results of replicate to file."))

	pRes = tryCatch({
		cat(sprintf("%d\t%d\t%s\t%d\t%d\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\n", 
		    	cur,
		    	n,
			type,
			pk,
			pk,
			init_mO,
			init_SDO,
			init_mD,
			init_SDD,
		    	true_mO,
		    	true_SDO,
		    	true_mD,
			true_mD_scaled,
		    	true_SDD,
			true_mC,
			true_SDC,
			trueEarliest,
			truePeak,
			e_first_start_time[1],
			e_first_start_time[2],
			e_first_start_time[3],
			overlap,
			cd,
			hMean_mO,
			hSigma_mO,
			hMean_SDO,
			hSigma_SDO,
			hMean_mD,
			hMean_mD_scaled,
			hSigma_mD,
			hMean_SDD,
			hSigma_SDD,
			est_mO_MAP,
			est_SDO_MAP,
			est_mD_MAP,
			est_SDD_MAP,
			est_peak_MAP,
			MAPEarliestEst[1],
			MAPEarliestEst[2],
			MAPEarliestEst[3],
			est_mO_MLE,
			est_SDO_MLE,
			est_mD_MLE,
			est_SDD_MLE,
			est_peak_MLE,
			MLEEarliestEst[1],
			MLEEarliestEst[2],
			MLEEarliestEst[3],
			est_mO_Stan,
			est_mD_Stan,
			est_mC_Stan,
			est_SD_Stan,
			est_mO_Stan_q2.5,
			est_mD_Stan_q2.5,
			est_mC_Stan_q2.5,
			est_SD_Stan_q2.5,
			est_mO_Stan_q97.5,
			est_mD_Stan_q97.5,
			est_mC_Stan_q97.5,
			est_SD_Stan_q97.5,
			StanEarliestEst[1],
			StanEarliestEst[2],
			StanEarliestEst[3],
			est_peak_Stan,
			est_mO_Stan_MAP,
			est_mD_Stan_MAP,
			est_mC_Stan_MAP,
			est_SD_Stan_MAP,
			StanEarliestEst_MAP[1],
			StanEarliestEst_MAP[2],
			StanEarliestEst_MAP[3],
			est_peak_Stan_MAP,
		    	q["10%"],
		    	q["50%"],
		    	q["90%"],
			pfe_f,
			plci_f,
			puci_f,
			pfe_s,
			plci_s,
			puci_s
		    	),file=outputFile, append=T)
	}, error = function(e) {
		message <- conditionMessage(e)
		print(paste("Print statement failed. Retrying. Error: ", message))
		cur = cur-1

	})
	cur = cur+1
	}	#top while loop
} # end analysis function

