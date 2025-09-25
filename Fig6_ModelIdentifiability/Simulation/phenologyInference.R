library(cmdstanr)
library(bayesplot)
library(tidyverse)
library(posterior)
library(here)

source("phenologyDistributions.R")

#______________________________________Code for GP

#get area of overlap between onset and cessation
getOverlap_GP = function(mu_O, mu_C, sigma, min=0, max=365) {
	
	#Analytic approximation for normal distributions from Inman, H.F. & Bradley Jr, E.L. (1989). The overlapping coefficient as a measure of agreement between probability distributions and point estimation of the overlap of two normal densities, Communications in Statistics - Theory and Methods, 18:10, 3851-3874, DOI: 10.1080/03610928908830127
	#see also: https://www.jstage.jst.go.jp/article/jjss1970/24/2/24_2_169/_pdf which seems to be the below value from chatgpt, requires equal means, I believe:
	#d <- (mu_O-mu_C)/sigma
	#return(2 * pnorm(-abs(d)/2))


        f <- function(x) dnorm(x, mu_O, sigma)
        g <- function(x) dnorm(x, mu_C, sigma)

        overlap <- integrate(function(x) pmin(f(x), g(x)), min,max)$value
	print(paste("Get overlap: ", mu_O, ", ", mu_C, ", ", sigma, ": ", overlap))
        return(overlap)
        }

#get the time of peak phenophase for the GP process, the observed distribution will be symmetrical - but this is worth checking
getPeakPhenophase_GP <- function(mu_o, mu_c) {
  #obj <- function(t) -dobserved_GP(t, mu_o, mu_c, sd)
  #peak = optimize(obj, interval = c(min, max))$minimum
	peak = mu_o + (mu_c - mu_o) / 2
  return(peak)
}

#Expected first onset time including 95% CI
E_first_start_time_GP = function(N, mu_O, sigma, min=0, max=1, verbose=F) {

	integrand <- function(x) { 
		x * dfirst_start_time_GP(x, N=N, mu_O=mu_O, sigma=sigma) 
	} 
	expected = integrate(integrand, min, max)$value 

	if(verbose) { print(paste("The expected first onset time is ", expected)) }
	return(c(expected,first_start_time_ci_GP(N, mu_O=mu_O, sigma=sigma,alpha=0.05)))
}

#Get confidence interval for first onset time
first_start_time_ci_GP <- function(N, mu_O, sigma,alpha=0.05) {
  c(lower = qfirst_start_time_GP(alpha/2,N, mu_O, sigma), upper = qfirst_start_time_GP(1-alpha/2,N,mu_O, sigma))
}

#Expected last cessation time including 95% CI
E_last_end_time_GP = function(N, mu_C, sigma, lower=0, upper=1) {
  integrand <- function(x) {
          x * dlast_end_time_GP(x, N=N, mu_C=mu_C, sigma=sigma)
  }
  expected = integrate(integrand, lower, upper)$value
  return(c(expected,last_end_time_ci_GP(N, mu_C, sigma, alpha=0.05)))
}

#Get confidence interval for last cessation time
last_end_time_ci_GP <- function(N, mu_C, sigma, alpha=0.05) {
  c(lower = qlast_end_time_GP(alpha/2,N, mu_C, sigma), upper = qlast_end_time_GP(1-alpha/2,N,mu_C, sigma))
}

expected_observed_GP = function(mu_O, mu_C) {
        meanO = mu_O + (mu_C - mu_O)/2
        return(meanO)
}

#Handled automatically: 
	#scaling the observations between 0 and 1
	#determination of the range in the covariate
	#scaling and translating the covariate so that the min is 0 and the max is 1
	#with the hyperparameter file information
		#hyperparameter file provides for each covariate:
			#mean and SD
			#one covariate per line
runCovariateGPPhenology = function(response, minResponse=0, maxResponse=365, onsetCovariates, durationCovariates, onsetHyperBeta, onsetHyperAnchor, durationHyperBeta, durationHyperAnchor, sigmaHyper=c(10,50), dataProvided=F, maxPercentDivergent=0.1, param_init_func, ...) {
print("Starting run")
	options(mc.cores = 4)
	min = minResponse
	max = maxResponse
	range = max-min

	#prepare the data
	# read from a file, if necessary
	# min max scale each covariate
	# scale observed values between user-supplied min and max

print("Getting data")
	if(dataProvided) {
		observed = (response - min ) / (max - min)
		covariatesOnset = processCovariates(onsetCovariates)
		covariatesDuration = processCovariates(durationCovariates)
	}
	else {
		#get scaled data
		observed = getObservations(response,min=min,max=max)
		covariatesOnset = getCovariates(onsetCovariates) #min max scaled 
		covariatesDuration = getCovariates(durationCovariates) #min max scaled 

		#get unscaled hyperparameters
		onsetHyperBeta = getHyperparameters(onsetHyperBeta)
		onsetHyperAnchor = getHyperparameters(onsetHyperAnchor)
		durationHyperBeta = getHyperparameters(durationHyperBeta)
		durationHyperAnchor = getHyperparameters(durationHyperAnchor)
	}

#print(covariatesOnset)
#print(covariatesDuration)
	K_O = covariatesOnset$K
	K_D = covariatesDuration$K

	#get the number of covariates for onset and for duration
	N = length(observed)
	N_O = nrow(covariatesOnset$scaledCovariates)
	N_D = nrow(covariatesDuration$scaledCovariates)

#print(covariatesOnset$scaledCovariates)

#print(N_O)
#print(N_D)

print("Testing data compatibility")
#check data dimensions
        if(N != N_O || N != N_D) {
                return(list(error=T, error_m="Be sure the sample size is the the same for the response (observed) values, for the covariates for the onset, and for the covariates for the duration. Rows should be parallel. Row 1 in the response file / data frame should correspond to the same individual in row 1 of each covariate file / data frame. If files names are input, corresponding files should be text formatted with tab-separated columns which should have headers. If data frames are input, these should have column names."))
}

#check hyperparameter specifications
	if(nrow(onsetHyperBeta) != K_O || ncol(onsetHyperBeta) != 2 || nrow(durationHyperBeta) != K_D || ncol(durationHyperBeta) != 2 || length(onsetHyperAnchor) != 2 || length(durationHyperAnchor) != 2 || length(sigmaHyper) != 2) {
                return(list(error=T, error_m="The input hyperparameter information should be a data frame with one row for each covariate, or provide a file name with corresponding file having the data frame as a text, tab-separated table. Each row should have two columns. The first column provides the mean value of the parameter's prior distribution, and the second column provides the SD of the parameter's prior distribtuion. The covariates in rows, top to bottom, should match the covariates in columns, left to right, in the input covariate data frames. Values should be in the original scale (e.g., units of days, or for slopes, total days changed over range of the covariate) of the observations. If files names are input, corresponding files should have headers. If data frames are input, these should have column labels of 'mean
 and 'sd'."))

}
print("Setting hyperparameters")
	covariatesOnsetRanges = covariatesOnset$maxs - covariatesOnset$mins	
	covariatesDurationRanges = covariatesDuration$maxs - covariatesDuration$mins	

	meanSlopeO = (onsetHyperBeta[[1]]/range) * covariatesOnsetRanges
	sdSlopeO = (onsetHyperBeta[[2]]/range) * covariatesOnsetRanges
	meanAnchorO = ((onsetHyperAnchor[1]-min)/range)
	sdAnchorO = (onsetHyperAnchor[2]/range)

	meanSlopeD = (durationHyperBeta[[1]]/range) * covariatesDurationRanges
	sdSlopeD = (durationHyperBeta[[2]]/range) * covariatesDurationRanges
	meanAnchorD = ((durationHyperAnchor[1])/range)
	sdAnchorD = (durationHyperAnchor[2]/range)

	meanSigma = (sigmaHyper[1]/range)
	sdSigma = (sigmaHyper[2]/range)

print("Preparing data for Stan")
	stanData <- list(
  	N = N,
	K_O = K_O,
	K_D = K_D,

  	t = observed,
	X_O = as.matrix(covariatesOnset$scaledCovariates),
	X_D = as.matrix(covariatesDuration$scaledCovariates),
	
	betaOnsetMeans = meanSlopeO,
	betaOnsetSDs = sdSlopeO, 
	anchorOnsetMean = meanAnchorO,
	anchorOnsetSD = sdAnchorO,

	betaDurationMeans = meanSlopeD,
	betaDurationSDs = sdSlopeD, 
	anchorDurationMean = meanAnchorD,
	anchorDurationSD = sdAnchorD,

	sigmaMean = meanSigma,
	sigmaSD = sdSigma,

  	debug = FALSE,
  	drop_nc = FALSE,
  	drop_ll = FALSE
	)

        #Attempt to compile Stan model
        print("Attempting to compile Stan model")
        m = tryCatch({
                cmdstan_model("gp.covariates.normalPrior.stan", stanc_options = list("allow-undefined"),  user_header = here::here("gp.hpp"))
        }, error = function(e) {
		message <- conditionMessage(e)
		print(paste("Error compiling Stan model: ", message))
                ret = list(
                           error = T,
                           errorM = paste(e$messageA, ": Could not compile Stan model in file gp.covariates.normalPrior.stan")
                           )
                return(ret)
        })


        #Attempt to run Stan model
print("Attempting to run Stan")
        res = tryCatch({
                if(missing(param_init_func)) {
                        m$sample(stanData,...)
                }
                else {
                        m$sample(stanData,init=param_init_func,...)
                }
        }, error = function(e) {
		print("ERROR")
                ret = list(
                           error = T,
                           errorM = paste(e$messageA, ": Could not sample Stan model"),
			   data = stanData
                           )
                return(ret)
        })

	#do the following in the R code that calls this function:
        	#extract the means and quantiles of the parameter estimates
        	#rescale the parameter estimates to the original scale
        	#print the output

	output = list(
		result=res,
		model=m,
		error = F,
		error_m = "No error was detected during the Stan run"
		)
        return(output)
}

#Each hyperparameter is a vector of two elements: the mean and the standard deviation of the normal distribution representing the prior distribution of the parameter. 
#The anchor is the mean value of the parameter at the mid point of the covariate, estimated as the average across the range of the covariate
runOneCovariateGPPhenology = function(response, minResponse=0, maxResponse=365, covariates, hyperSlopeO, hyperSlopeD, hyperAnchorO, hyperAnchorD, hyperSigma, dataProvided=F, param_init_func, ...) {
	options(mc.cores = 4)
	min = minResponse
	max = maxResponse
	range = max-min

	#prepare the data
	# read from a file, if necessary
	# min max scale each covariate
	# scale observed values between user-supplied min and max
	if(dataProvided) {
		observed = (response - min ) / (max - min)
		covariates = processCovariates(covariates)
	}
	else {
		observed = getObservations(response,min=min,max=max)
		covariates = getCovariates(covariates) #min max scaled 
	}

	#get the number of covariates for onset and for duration
	N = length(observed)

	if(N != nrow(covariates$scaledCovariates)) {
		return(list(error=T, error_m="Be sure the sample size is the the same for the response (observed) values, for the covariates for the onset, and for the covariates for the duration"))
	}

	covariatesRange = covariates$maxs[1] - covariates$mins[1]

	meanSlopeO = (hyperSlopeO[1]/range) * covariatesRange
	sdSlopeO = (hyperSlopeO[2]/range) * covariatesRange
	meanAnchorO = ((hyperAnchorO[1]-min)/range)
	sdAnchorO = (hyperAnchorO[2]/range)

	meanSlopeD = (hyperSlopeD[1]/range) * covariatesRange
	sdSlopeD = (hyperSlopeD[2]/range) * covariatesRange
	meanAnchorD = (hyperAnchorD[1]/range)
	sdAnchorD = (hyperAnchorD[2]/range)

	meanSigma = (hyperSigma[1]/range)
	sdSigma = (hyperSigma[2]/range)

	#print(paste("Mean Slope Onset: ", meanSlopeO))
	#print(paste("SD Slope Onset: ", sdSlopeO))
	#print(paste("Mean Anchor Onset: ", meanAnchorO))
	#print(paste("SD Anchor Onset: ", sdAnchorO))
	#print(paste("Mean Slope Duration: ", meanSlopeD))
	#print(paste("SD Slope Duration: ", sdSlopeD))
	#print(paste("Mean Anchor Duration: ", meanAnchorD))
	#print(paste("SD Anchor Duration: ", sdAnchorD))
	#print(paste("Mean Sigma: ", meanSigma))
	#print(paste("SD Sigma: ", sdSigma))

	#set up the data for Stan
	stanData <- list(
  	N = N,
  	t = observed,
	X = covariates$scaledCovariates[[1]],
	
	meanSlopeO = (hyperSlopeO[1]/range) * covariatesRange,
	sdSlopeO = (hyperSlopeO[2]/range) * covariatesRange,
	meanAnchorO = ((hyperAnchorO[1]-min)/range),
	sdAnchorO = (hyperAnchorO[2]/range),

	meanSlopeD = (hyperSlopeD[1]/range) * covariatesRange,
	sdSlopeD = (hyperSlopeD[2]/range) * covariatesRange,
	meanAnchorD = (hyperAnchorD[1]/range),
	sdAnchorD = (hyperAnchorD[2]/range),

	meanSigma = (hyperSigma[1]/range),
	sdSigma = (hyperSigma[2]/range),

  	debug = FALSE,
  	drop_nc = FALSE,
  	drop_ll = FALSE
	)

	#Attempt to compile Stan model
        print("Attempting to compile Stan model")
        m = tryCatch({
                cmdstan_model("gp.oneCovariate.stan", stanc_options = list("allow-undefined"),  user_header = here::here("gp.hpp"))
        }, error = function(e) {
                ret = list(
                           error = T,
                           errorM = paste(e$messageA, " Could not compile Stan model in file gp.oneCovariate.stan")
                           )
                return(ret)
        })


        #Attempt to run Stan model
        res = tryCatch({
                if(missing(param_init_func)) {
                        return(m$sample(stanData,...))
                }
                else {
                        return(m$sample(stanData,init=param_init_func,...))
                }
        }, error = function(e) {
                ret = list(
                           error = T,
                           errorM = paste(e$messageA, " Could sample Stan model.")
                           )
                return(ret)
        })

	#do the following in the R code that calls this function:
        	#extract the means and quantiles of the parameter estimates
        	#rescale the parameter estimates to the original scale
        	#print the output

        return(res)


}



runCovariateGPPhenology_uniformPrior = function(responseFile, min=0, max=365, onsetCovariatesFile, durationCovariatesFile, dataProvided=F, maxPercentDivergent=0.1, param_init_func, ...) {

	#prepare the data
	# read from a file, if necessary
	# min max scale each covariate
	# scale observed values between user-supplied min and max
	if(dataProvided) {
		observed = (responseFile - min ) / (max - min)
		covariatesOnset = processCovariates(onsetCovariatesFile)
		covariatesDuration = processCovariates(durationCovariatesFile)
	}
	else {
		observed = getObservations(responseFile,min=min,max=max)
		covariatesOnset = getCovariates(onsetCovariatesFile)
		covariatesDuration = getCovariates(durationCovariatesFile)
	}

	#get the number of covariates for onset and for duration
	N = length(observed)

	K_O = ncol(covariatesOnset$covariates)
	N_O = nrow(covariatesOnset$covariates)
	#M_O = as.matrix(covariatesOnset$scaledCovariates,ncol=K_O)
	M_O = as.matrix(covariatesOnset$covariates,ncol=K_O)

	K_D = ncol(covariatesDuration$covariates)
	N_D = nrow(covariatesDuration$covariates)
	#M_D = as.matrix(covariatesDuration$scaledCovariates,ncol=K_O)
	M_D = as.matrix(covariatesDuration$covariates,ncol=K_O)

	print(nrow(covariatesOnset$covariates))
	print(nrow(covariatesDuration$covariates))
	print(N)

	print(M_O)


	if(N != N_O || N != N_D) {
		return(list(error=T, error_m="Be sure the sample size is the the same for the response (observed) values, for the covariates for the onset, and for the covariates for the duration"))
	}

	#set up the hyperparameter ranges
	#betaMin_O = rep(feasible_beta_range(0,1,min(observed),max(observed))[1], K_O)
	#betaMin_D = rep(feasible_beta_range(0,1,min(observed),max(observed))[1], K_D)

	#betaMax_O = rep(feasible_beta_range(0,1,min(observed),max(observed))[2], K_O)
	#betaMax_D = rep(feasible_beta_range(0,1,min(observed),max(observed))[2], K_D)

	betaMin_O = rep(feasible_beta_range(0,1,min(observed),max(observed))[1], K_O)
	betaMin_D = rep(feasible_beta_range(0,1,min(observed),max(observed))[1], K_D)

	betaMax_O = rep(feasible_beta_range(0,1,min(observed),max(observed))[2], K_O)
	betaMax_D = rep(feasible_beta_range(0,1,min(observed),max(observed))[2], K_D)

	print(betaMin_O)
	print(betaMax_O)

#stop("Debug")

	#set up the data for Stan
	stanData <- list(
  	N = N,
  	t = observed,
  	K_D = K_D,
  	K_O = K_O,
  	X_D = M_D,
  	X_O = M_O,
	betaMin_O = betaMin_O, 	#get the minimum feasible slope - all covariates are scaled between 0 and 1
	betaMax_O = betaMax_O, 	#get the maximum feasible slope - all covariates are scaled between 0 and 1
	betaMin_D = betaMin_D, 	#get the minimum feasible slope - all covariates are scaled between 0 and 1
	betaMax_D = betaMax_D, 	#get the maximum feasible slope - all covariates are scaled between 0 and 1
  	debug = FALSE,
  	drop_nc = FALSE,
  	drop_ll = FALSE
	)
	
	#Attempt to compile Stan model
        print("Attempting to compile Stan model")
        m = tryCatch({
                cmdstan_model("gp.covariates.uniformPrior.stan", stanc_options = list("allow-undefined"),  user_header = here::here("gp.hpp"))
        }, error = function(e) {
                ret = list(
                           error = T,
                           errorM = paste(e$messageA, " Could not compile Stan model in file gp.covariates.uniformPrior.stan")
                           )
                return(ret)
        })


	#Attempt to run Stan model
	res = tryCatch({
		if(missing(param_init_func)) {
			return(m$sample(stanData,...))
		}
		else {
			return(m$sample(stanData,init=param_init_func,...))
		}
	}, error = function(e) {
                ret = list(
                           error = T,
                           errorM = paste(e$messageA, " Could not compile Stan model in file gp.covariates.uniformPrior.stan")
                           )
                return(ret)
	})

	#extract the means and quantiles of the parameter estiamtes

	#rescale the parameter estimates to the original scale

	#print the output

	return(res)
}

runUnivariateGPPhenology_standardized = function(fileOrData, hyperparameters = c(100,7,50,7,10,7), dataProvided=F, maxPercentDivergent=100, runMAP=F, ...) {
	options(mc.cores = 4)

	print("starting Stan run")
	#get and scale observed times
	if(dataProvided) {
		observed = fileOrData
		responseMean = mean(observed)
		responseSD = sd(observed)
		observed = (observed - responseMean ) / responseSD
	}
	else {
	observed_list = getObservations_standardized(fileOrData)
	observed = observed_list$observed
	responseMean = observed_list$mean
	responseSD = observed_list$sd
	}

	#set the hyperparameters
	#hyperparameters_Stan = c(hMean_mO,hSigma_mO,hMean_mD_scaled,hSigma_mD,hMean_SDO,hSigma_SDO)
	mean_mean_onset = (hyperparameters[1] - responseMean) / responseSD
	sd_mean_onset = hyperparameters[2] / responseSD
	mean_mean_duration = hyperparameters[3] / responseSD
	sd_mean_duration = hyperparameters[4] / responseSD
	mean_sd = hyperparameters[5] / responseSD 		#taken just from the onset distribution sigma, as, in the GP model, duration is deterministic 
	sd_sd = hyperparameters[6] / responseSD 			#taken just from the onset distribution sigma, as, in the GP model,  duration is deterministic

	#prepare data for Stan
	data <- list(
  		N = length(observed),
  		t = observed,
  		#sigma = sigma,
  		debug = FALSE,
  		drop_nc = FALSE,
  		drop_ll = FALSE,
		responseMean = responseMean,
		responseSD = responseSD,
  		mean_mean_onset = mean_mean_onset,
  		sd_mean_onset = sd_mean_onset,
  		mean_mean_duration = mean_mean_duration,
  		sd_mean_duration = sd_mean_duration,
  		mean_sd = mean_sd,
  		sd_sd = sd_sd
	)
	data$debug <- 0

	#compile stan model
	print("attempting to compile Stan model")
	m = tryCatch({
		cmdstan_model("gpU.standardized.stan", stanc_options = list("allow-undefined"),  user_header = here::here("gp.hpp"))
	}, error = function(e) {
		ret = list(
			   error = T,
			   errorM = e$messageA
			   )
		return(ret)
	})


	#sample the posterior based on the provided observations and hyperparameter values
	print("attempting to sample Stan model")
	res = tryCatch({
		m$sample(data)
	}, error = function(e) {
		ret = list(
			   error = T,
			   errorM = e$messageA
			   )
		return(ret)
	})


	sampler_diagnostics <- res$sampler_diagnostics()
	divergent_array <- sampler_diagnostics[, , "divergent__"]
	divergent_vector <- as.vector(divergent_array)
	percent_divergent <- mean(divergent_vector) * 100.0

	if(maxPercentDivergent < 100) {	
		if(percent_divergent > maxPercentDivergent) {
			ret = list(
			   	error = T,
			   	errorM = paste("Percent divergent was too high: ", percent_divergent)
			   	)
			return(ret)
		}
	}


	if(runMAP) {
	#get GP model MAP
	print("attempting to optimize Stan model")
	resMAP = tryCatch({
		chain <- 1
		iter <- 2000

		init_f_samp <- function() { 
			init_df <- res$draws() %>% as_draws_df() %>% filter(.chain == chain, .iteration == iter) 
			as.list(init_df) 
		}

		#m$optimize(data, init = init_f_samp)

		m$optimize(data)
	}, error = function(e) {
		ret = list(
			   error = T,
			   errorM = e$messageA
			   )
		return(ret)
	})

	print("done optimizing Stan model")
	}

	#pull out the results
	#df = summary(res$draws(),quantiles = ~ quantile2(., probs = c(0.025, 0.975))) %>% as.data.frame()
	#making table with 95% CI (rather than default 90% CI)
	df = summary(res$draws(),posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()

	mO = df[df$variable=="mu_O_orig",]$mean
	mO.q2.5 = df[df$variable=="mu_O_orig",]$q2.5
	mO.q97.5 = df[df$variable=="mu_O_orig",]$q97.5
	mD = df[df$variable=="mu_D_orig",]$mean
	mD.q2.5 = df[df$variable=="mu_D_orig",]$q2.5
	mD.q97.5 = df[df$variable=="mu_D_orig",]$q97.5
	mC = df[df$variable=="mu_C_orig",]$mean
	mC.q2.5 = df[df$variable=="mu_C_orig",]$q2.5
	mC.q97.5 = df[df$variable=="mu_C_orig",]$q97.5
	sigma = df[df$variable=="sigma_orig",]$mean
	sigma.q2.5 = df[df$variable=="sigma_orig",]$q2.5
	sigma.q97.5 = df[df$variable=="sigma_orig",]$q97.5

	par = c(mO,mD,mC,sigma)
	par_q2.5 = c(mO.q2.5,mD.q2.5,mC.q2.5,sigma.q2.5)
	par_q97.5 = c(mO.q97.5,mD.q97.5,mC.q97.5,sigma.q97.5)

	if(runMAP) {
	print("got stan HMC estimates. moving onto stan MAP estimates.")
	df = resMAP$summary() %>% as.data.frame()
	mO_MAP = df[df$variable=="mu_O_orig",]$estimate
	mD_MAP = df[df$variable=="mu_D_orig",]$estimate
	mC_MAP = df[df$variable=="mu_C_orig",]$estimate
	sigma_MAP = df[df$variable=="sigma_orig",]$estimate

	par_MAP = c(mO_MAP, mD_MAP, mC_MAP, sigma_MAP)
	print("got stan MAP estimates.")
	}
	else {
		par_MAP = c(NA,NA,NA,NA)
	}

	print("compiling results of Stan model run")

	result = list(
		      model = m, 				#Stan model
		      sample = res,				#HMC sample
		      data = data,				#original data provided to Stan
		      par = par,				#estimate of parmeters
                      par_q2.5 = par_q2.5,
                      par_q97.5 = par_q97.5,
		      par_MAP = par_MAP,	#est of parmeters orig scale MAP
		      error = F,
		      errorM = "Results were obtained, but check diagnostics of the sample."
		      )
	return(result)
}


#hyperparameters are 
#	mean of Onset mean
#	sd of Onset mean
#	mean of Duration mean
#	sd of Duration mean
#	mean of Sigma
#	sd of Sigma
#	currently, the ... are not implemented

runUnivariateGPPhenology = function(fileOrData, min=0, max=365, hyperparameters = c(100,7,50,7,10,7), dataProvided=F, maxPercentDivergent=100, runMAP=T, nRej=0, ...) {
	options(mc.cores = 4)

	print("starting Stan run")
	#get and scale observed times
	if(dataProvided) {
		observed = (fileOrData - min ) / (max - min)
	}
	else {
	observed = getObservations(fileOrData,min=min,max=max)
	}

	#set the hyperparameters
	#hyperparameters_Stan = c(hMean_mO,hSigma_mO,hMean_mD_scaled,hSigma_mD,hMean_SDO,hSigma_SDO)
	mean_mean_onset = (hyperparameters[1] - min) / (max-min)
	sd_mean_onset = hyperparameters[2] / (max-min)
	mean_mean_duration = hyperparameters[3] / (max-min)
	sd_mean_duration = hyperparameters[4] / (max-min)
	mean_sd = hyperparameters[5] / (max-min) 		#taken just from the onset distribution sigma, as, in the GP model, duration is deterministic 
	sd_sd = hyperparameters[6] / (max-min) 			#taken just from the onset distribution sigma, as, in the GP model,  duration is deterministic

	#prepare data for Stan
	data <- list(
  		N = length(observed),
  		t = observed,
  		#sigma = sigma,
  		debug = FALSE,
  		drop_nc = FALSE,
  		drop_ll = FALSE,
  		mean_mean_onset = mean_mean_onset,
  		sd_mean_onset = sd_mean_onset,
  		mean_mean_duration = mean_mean_duration,
  		sd_mean_duration = sd_mean_duration,
  		mean_sd = mean_sd,
  		sd_sd = sd_sd
	)
	data$debug <- 0

	#compile stan model
	print("attempting to compile Stan model")
	m = tryCatch({
		cmdstan_model("gpU.stan", stanc_options = list("allow-undefined"),  user_header = here::here("gp.hpp"))
	}, error = function(e) {
		ret = list(
			   error = T,
			   errorM = e$messageA
			   )
		return(ret)
	})


	#sample the posterior based on the provided observations and hyperparameter values
	print("attempting to sample Stan model")
	res = tryCatch({
		if(nRej>20) {
			m$sample(data = data, adapt_delta = 0.99, max_treedepth = 15)
		}
		else {
			m$sample(data = data)
		}
	}, error = function(e) {
		ret = list(
			   error = T,
			   errorM = e$messageA
			   )
		return(ret)
	})


	sampler_diagnostics <- res$sampler_diagnostics()
	divergent_array <- sampler_diagnostics[, , "divergent__"]
	divergent_vector <- as.vector(divergent_array)
	percent_divergent <- mean(divergent_vector) * 100

	if(maxPercentDivergent < 100) {	
		if(percent_divergent > maxPercentDivergent) {
			ret = list(
			   	error = T,
			   	errorM = paste("Percent divergent was too high: ", percent_divergent)
			   	)
			return(ret)
		}
	}


	if(runMAP) {
	#get GP model MAP
	print("attempting to optimize Stan model")
	resMAP = tryCatch({
		chain <- 1
		iter <- 200

		init_f_samp <- function() { 
			init_df <- res$draws() %>% as_draws_df() %>% filter(.chain == chain, .iteration == iter) 
			as.list(init_df) 
		}

		#m$optimize(data, init = init_f_samp)

		m$optimize(data)
	}, error = function(e) {
		ret = list(
			   error = T,
			   errorM = e$messageA
			   )
		return(ret)
	})

	print("done optimizing Stan model")
	}

	#pull out the results
	#df = summary(res$draws(),quantiles = ~ quantile2(., probs = c(0.025, 0.975))) %>% as.data.frame()
	#making table with 95% CI (rather than default 90% CI)
	df = summary(res$draws(),posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()

	mO = df[df$variable=="mu_O",]$mean
	mO.q2.5 = df[df$variable=="mu_O",]$q2.5
	mO.q97.5 = df[df$variable=="mu_O",]$q97.5
	mD = df[df$variable=="mu_D",]$mean
	mD.q2.5 = df[df$variable=="mu_D",]$q2.5
	mD.q97.5 = df[df$variable=="mu_D",]$q97.5
	mC = df[df$variable=="mu_C",]$mean
	mC.q2.5 = df[df$variable=="mu_C",]$q2.5
	mC.q97.5 = df[df$variable=="mu_C",]$q97.5
	sigma = df[df$variable=="sigma",]$mean
	sigma.q2.5 = df[df$variable=="sigma",]$q2.5
	sigma.q97.5 = df[df$variable=="sigma",]$q97.5

	par = c(mO,mD,mC,sigma)
	par_orig_scale = c(mO=min + mO * (max-min), mD=mD * (max-min), mC=min + mC * (max-min), sigma=sigma * (max-min))
	par_orig_scale_q2.5 = c(mO.q2.5=min + mO.q2.5 * (max-min), mD.q2.5=mD.q2.5 * (max-min), mC.q2.5=min + mC.q2.5 * (max-min), sigma.q2.5=sigma.q2.5 * (max-min))
	par_orig_scale_q97.5 = c(mO.q97.5=min + mO.q97.5 * (max-min), mD.q97.5=mD.q97.5 * (max-min), mC.q97.5=min + mC.q97.5 * (max-min), sigma.q97.5=sigma.q97.5 * (max-min))

	if(runMAP) {
	print("got stan HMC estimates. moving onto stan MAP estimates.")
	df = resMAP$summary() %>% as.data.frame()
	mO_MAP = min + df[df$variable=="mu_O",]$estimate * (max-min)
	mD_MAP = df[df$variable=="mu_D",]$estimate * (max-min)
	mC_MAP = min + df[df$variable=="mu_C",]$estimate * (max-min)
	sigma_MAP = df[df$variable=="sigma",]$estimate * (max-min)

	par_orig_scale_MAP = c(mO_MAP, mD_MAP, mC_MAP, sigma_MAP)
	print("got stan MAP estimates.")
	}
	else {
		par_orig_scale_MAP = c(NA,NA,NA,NA)
	}

	print("compiling results of Stan model run")

	result = list(
		      model = m, 				#Stan model
		      sample = res,				#HMC sample
		      data = data,				#original data provided to Stan
		      par = par,				#estimate of parmeters
		      par_orig_scale = par_orig_scale,		#est of parmeters orig scale
		      par_orig_scale_q2.5 = par_orig_scale_q2.5,	#est of parmeters orig scale q2.5
		      par_orig_scale_q97.5 = par_orig_scale_q97.5,	#est of parmeters orig scale q97.5
		      par_orig_scale_MAP = par_orig_scale_MAP,	#est of parmeters orig scale MAP
		      error = F,
		      errorM = "Results were obtained, but check diagnostics of the sample."
		      )
	return(result)
}


#________________________________________________End code for GP

getCohensD = function(mean1, mean2, sd1, sd2) {
	sp = sqrt((sd1^2 + sd2^2)/2)
	d = (mean1-mean2)/sp
	return(d)
}

#get area of overlap between onset and cessation
getOverlap = function(alpha_s, beta_s, alpha_d, beta_d) {

        f <- function(x) dstart_time(x, alpha_s=alpha_s, beta_s=beta_s)
        g <- function(x) dend_time(x, alpha_s=alpha_s, beta_s=beta_s, alpha_d=alpha_d, beta_d=beta_d)

        overlap <- integrate(function(x) pmin(f(x), g(x)), 0.00001, 0.9999)$value
        return(overlap)
        }


#hyperparameters are the mean and sd for mean onset, mean duration, sd onset and sd duration 
# put initial values and hyperparameters in the original scale (e.g., 0 - 365 for days)
#results are in terms of the beta parameters (shape parameters for onset scaled between 0 and 1 and shape parameters for duration scaled between 0 and 1)
runMAPPhenology = function(fileOrData, min=0, max=365,minS=1, maxS=3000,  init_params = c(180,20,60,7), hyperparameters = c(100, 7, 60, 6, 24, 12, 24, 12), dataProvided=F) {

	if(length(hyperparameters) != 8) {
		print("Provide hyperparameters. Hyperparameters are the mean as sd for mean onset, mean duration, sd onset, and sd duration. So, there should be 8 hyperparameters total. Defaults are provided.")
		return()
	}

	#convert raw initial parameter values to beta parameters
	init_params1 = init_params
	init_params1[1] = (init_params1[1] - min)
	init_params1 = init_params1 / (max - min)
	init_params_beta = init_params1
	init_params_beta[1] =  beta_alpha(init_params1[1], init_params1[2])
	init_params_beta[2] =  beta_beta(init_params1[1], init_params1[2])
	init_params_beta[3] =  beta_alpha(init_params1[3], init_params1[4])
	init_params_beta[4] =  beta_beta(init_params1[3], init_params1[4])

	if(!isBetaFeasible(init_params_beta[1],init_params_beta[2], minS, maxS) || !isBetaFeasible(init_params_beta[3],init_params_beta[4], minS, maxS)) {
		print(init_params)
		print(init_params_beta)
		result = list(error = T, error_m = "Infeasible beta parameters")
		return(result)
	}

	#gets and scales times
	if(dataProvided) {
		observed = (fileOrData - min ) / (max - min)
	}
	else {
	observed = getObservations(fileOrData,min=min,max=max)
	}

	#generate hyperparameters
	hMean_mO = (hyperparameters[1] - min) / (max - min)
	hSigma_mO = hyperparameters[2] / (max - min)
	hMean_SDO = hyperparameters[5]  / (max - min)
	hSigma_SDO = hyperparameters[6] / (max - min)

	hMean_mD = hyperparameters[3] / (max - min)
	hSigma_mD = hyperparameters[4] / (max - min)
	hMean_SDD = hyperparameters[7] / (max - min)
	hSigma_SDD = hyperparameters[8] / (max - min)

        a_o_m = beta_alpha(hMean_mO, hSigma_mO)
        b_o_m = beta_beta(hMean_mO, hSigma_mO)
        a_d_m = beta_alpha(hMean_mD, hSigma_mD)
        b_d_m = beta_beta(hMean_mD, hSigma_mD)

        a_o_sd = beta_alpha(hMean_SDO, hSigma_SDO)
        b_o_sd = beta_beta(hMean_SDO, hSigma_SDO)
        a_d_sd = beta_alpha(hMean_SDD, hSigma_SDD)
        b_d_sd = beta_beta(hMean_SDD, hSigma_SDD)

	#check hyperparameter feasibility
	if(!isBetaFeasible(a_o_m, b_o_m,0.01,maxS) || !isBetaFeasible(a_d_m, b_d_m,0.01,maxS) || !isBetaFeasible(a_o_sd, b_o_sd,0.01,maxS) || !isBetaFeasible(a_d_sd, b_d_sd,0.01,maxS)) {
		print(paste("hm_mo: ", hMean_mO, " hsd_mo: ", hSigma_mO, " hm_sdo: ", hMean_SDO, " hsd_sdo: ", hSigma_SDO))
		print(c(a_o_m,b_o_m,a_o_sd,b_o_sd))
		print(paste("hm_md: ", hMean_mD, " hsd_md: ", hSigma_mD, " hm_sdd: ", hMean_SDD, " hsd_sdd: ", hSigma_SDD))
		print(c(a_d_m,b_d_m,a_d_sd,b_d_sd))
		result = list(error = T, error_m = "Infeasible beta hyperparameters")
		return(result)
	}

	print("Starting MAP optimization")
#neg_log_posterior_origScale <- function(param, t_obs, a_o_m, b_o_m, a_d_m, b_d_m, a_o_sd, b_o_sd, a_d_sd, b_d_sd,min=0, max=365) {

	result <- optim(
  	par = init_params_beta,
  	#par = init_params,
  	fn = neg_log_posterior,
  	#fn = neg_log_posterior_origScale,
  	t_obs = observed,
  	a_o_m = a_o_m, b_o_m = b_o_m, a_d_m = a_d_m, b_d_m = b_d_m,	#beta priors for the means
  	a_o_sd = a_o_sd, b_o_sd = b_o_sd, a_d_sd = a_d_sd, b_d_sd = b_d_sd,	#beta priors for the SDs
  	method = "L-BFGS-B",
  	lower = c(minS,minS,minS,minS),
  	upper = c(maxS,maxS,maxS,maxS)
  	#lower = c(min,min,min,min),
  	#upper = c(max,max,max,max)
	)

	print("Done MAP optimization")

	par = result$par
	mO = min + beta_mean(par[1],par[2]) * (max - min)
	sdO = beta_sd(par[1],par[2]) * (max - min)
	mD = beta_mean(par[3],par[4]) * (max - min)
	sdD = beta_sd(par[3],par[4]) * (max - min)
	#mO = par[1]
	#sdO = par[2]
	#mD = par[3]
	#sdD = par[4]
	result$par_orig_scale = c(mO, sdO, mD, sdD)
	result$error = F

	if(result$convergence != 0) {
	result$error = T
	result$error_m = "optimization failed to converged."
	}

	return(result)
}

#init_params is a vector with mean onset, sd onset, mean duration (prescaled), sd duration
runMLEPhenology = function(fileOrData, min=0, max=365, minS=1, maxS=3000, init_params = c(180, 20, 60, 7), dataProvided=F) {

	#convert raw initial parameter values to beta parameters
	init_params1 = init_params
	init_params1[1] = (init_params[1] - min)
	init_params1 = init_params / (max - min)
	init_params_beta = init_params1;
	init_params_beta[1] =  beta_alpha(init_params1[1], init_params1[2])
	init_params_beta[2] =  beta_beta(init_params1[1], init_params1[2])
	init_params_beta[3] =  beta_alpha(init_params1[3], init_params1[4])
	init_params_beta[4] =  beta_beta(init_params1[3], init_params1[4])

	if(!isBetaFeasible(init_params_beta[1],init_params_beta[2], minS, maxS) || !isBetaFeasible(init_params_beta[3],init_params_beta[4], minS, maxS))
	{
		result = list(error = T, error_m = "Infeasible beta parameters")
		return(result)
	}

	#gets and scales times
	if(dataProvided) {
		observed = (fileOrData - min ) / (max - min)
	}
	else {
	observed = getObservations(fileOrData,min=min,max=max)
	}



	result <- optim(
  	par = init_params_beta,
  	#par = init_params,
  	#fn = neg_loglik_observed_origScale,
  	fn = neg_loglik_observed,
  	data = observed,
  	method = "L-BFGS-B",
  	lower = c(minS,minS,minS,minS),
  	upper = c(maxS,maxS,maxS,maxS)
  	#lower = c(min,min,min,min),
  	#upper = c(max,max,max,max)
	)
	par = result$par
	mO = min + beta_mean(par[1],par[2]) * (max - min)
	sdO = beta_sd(par[1],par[2]) * (max - min)
	mD = beta_mean(par[3],par[4]) * (max - min)
	sdD = beta_sd(par[3],par[4]) * (max - min)
	#mO = par[1]
	#sdO = par[2]
	#mD = par[3]
	#sdD = par[4]
	result$par_orig_scale = c(mO, sdO, mD, sdD)
	result$error = F

	if(result$convergence != 0) {
	result$error = T
	}

	return(result)
}



#not as good
#runMLEPhenology_fitdist = function(responseFile, min=0, max=365,start = list(alpha_s = 10, alpha_d = 10, beta_s = 10, beta_d = 10), ...) {
#
	#observed = getObservations(responseFile,min=min,max=max)
	#fit = fitdist(data = observed, "observed", start = start, ...)
	#return(fit)
#}

runStanPhenology_wCovariates = function(responseFile, onsetCovariatesFile, durationCovariatesFile, beta_mean_onset, beta_sd_onset, alpha_mean_onset = 0, alpha_sd_onset = 50, beta_mean_duration, beta_sd_duration, alpha_mean_duration=0, alpha_sd_duration=50, mean_sigma=10, sd_sigma=10, parameters_init_func, min=0, max=365, ...) {
	if(missing(durationCovariatesFile) || missing(onsetCovariatesFile)) {
		print("Missing covariates files. Quitting.")
		return()
	}


	m <- cmdstan_model("/home/david/Documents/Projects/HerbariumConfounding/FinalAttempt/GP/Covariates/gp.covariates.stan", stanc_options = list("allow-undefined"),  user_header = "/home/david/Documents/Projects/HerbariumConfounding/FinalAttempt/GP/Covariates/gp.hpp")

	#get the observed phenophase times appropriately scaled
	observed = getObservations(responseFile,min=min,max=max)
	N = length(observed)

		#read the covariates file for duration
		#!!!!THESE MAY NEED TO BE SCALED BETWEEN 0 and 1
	
		duration = read.table(durationCovariatesFile, header=T, sep='\t')
		K_D = ncol(duration)
		N_D = nrow(duration)
		matrix_D = as.matrix(duration,ncol=K_D)
	
		#read the covariates file for onset
	
		onset = read.table(onsetCovariatesFile, header=T, sep='\t')
		K_O = ncol(onset)
		N_O = nrow(onset)
		matrix_O = as.matrix(duration,ncol=K_O)

	if(missing(beta_mean_onset) || missing(beta_sd_onset) || missing(beta_mean_duration) || missing(beta_sd_duration) || missing(alpha_mean_onset)|| missing(alpha_sd_onset)|| missing(alpha_mean_duration)|| missing(alpha_sd_duration)|| missing(mean_sigma) || missing(sd_sigma)    ) {
	print("estimating hyperparameters from training data partition. This may take a while")
	hyperParams = getPriorHyperparameters(observed,duration,onset)
	beta_mean_onset = hyperParams[["beta_mean_onset"]]
	beta_sd_onset = hyperParams[["beta_sd_onset"]]
	alpha_mean_onset = hyperParams[["alpha_mean_onset"]]
	alpha_sd_onset = hyperParams[["alpha_sd_onset"]]
	beta_mean_duration = hyperParams[["beta_mean_duration"]]
	beta_sd_duration = hyperParams[["beta_sd_duration"]]
	alpha_mean_duration = hyperParams[["alpha_mean_duration"]]
	alpha_sd_duration = hyperParams[["alpha_sd_duration"]]
	mean_sigma = hyperParams[["mean_sigma"]]
	sd_sigma = hyperParams[["sd_sigma"]]
	parameters_init_func = hyperParams[["init"]]
	}
	else {
	beta_mean_onset = (beta_mean_onset - min)/(max-min)
	beta_sd_onset = (beta_sd_onset - min)/(max-min)
	alpha_mean_onset = (alpha_mean_onset - min)/(max-min)
	alpha_sd_onset = (alpha_sd_onset - min)/(max-min)
	beta_mean_duration = (beta_mean_duration - min)/(max-min)
	beta_sd_duration = (beta_sd_duration - min)/(max-min)
	alpha_mean_duration = (alpha_mean_duration - min)/(max-min)
	alpha_sd_duration = (alpha_sd_duration - min)/(max-min)
	mean_sigma = (mean_sigma - min)/(max-min)
	sd_sigma = (sd_sigma - min)/(max-min)
	}
	
		if(N != N_D || N != N_O) {
			print("The number of rows in the response file, onset covariates file, and duration covariates file must be the same. The header row should provide a meaningful label for each covariate present in each column.")
			return();
		}
	
		#set up the data for Stan
	
		#print(paste("preparing data for Stan: N=", N, " K_D=", K_D, " N_D=", N_D, " N_row_D=", nrow(matrix_D)))
	stanData <- list(
  	N = N,
  	t = observed,
  	K_D = K_D,
  	K_O = K_O,
  	X_D = matrix_D,
  	X_O = matrix_O,
	beta_mean_onset = beta_mean_onset,
	beta_sd_onset = beta_sd_onset,
	alpha_mean_onset = alpha_mean_onset,
	alpha_sd_onset = alpha_sd_onset,
	beta_mean_duration = beta_mean_duration,
	beta_sd_duration = beta_sd_duration,
	alpha_mean_duration = alpha_mean_duration,
	alpha_sd_duration = alpha_sd_duration,
	mean_sigma = mean_sigma,
	sd_sigma = sd_sigma,
	#sigma = sigma,
  	debug = FALSE,
  	drop_nc = FALSE,
  	drop_ll = FALSE
	)
	if(missing(parameters_init_func)) {
		res <- m$sample(stanData,...)
	}
	else {
		res <- m$sample(stanData,init=parameters_init_func,...)
	}
return(res)
}

getPriorHyperparameters = function(observed,duration,onset) {
	#return a list of the hyperparameters for the Stan inference with covariates
	#NOT SURE HOW TO GET HYPERPARAMETERS FOR DURATIONS


}



getPeakPhenophase <- function(mu_o, sd_o, mu_d, sd_d, min=0, max=1) {
	#print(c(mu_o,sd_o,mu_d,sd_d))
	mu_o = (mu_o - min) / (max-min)
	sd_o = sd_o / (max-min)
	mu_d = mu_d / (max-min)
	sd_d = sd_d / (max-min)
  alpha_s = beta_alpha(mu_o, sd_o)
  beta_s = beta_beta(mu_o, sd_o)
  alpha_d = beta_alpha(mu_d, sd_d)
  beta_d = beta_beta(mu_d, sd_d)

  obj <- function(t) -dobserved(t, alpha_s, beta_s, alpha_d, beta_d)
  peak = optimize(obj, interval = c(1e-4, 1 - 1e-4))$minimum
  return(min + peak * (max-min))
}


E_last_end_time = function(N, alpha_s, beta_s, alpha_d, beta_d, lower=0, upper=1) {
  integrand <- function(x) {
          x * dlast_end_time(x, N=N, alpha_s=alpha_s, beta_s=beta_s, alpha_d=alpha_d, beta_d=beta_d)
  }
  expected = integrate(integrand, lower, upper)$value
  return(c(expected,last_end_time_ci(N, alpha_s, beta_s, alpha_d, beta_d, alpha=0.05)))
}

last_end_time_ci <- function(N, alpha_s, beta_s, alpha_d, beta_d, alpha=0.05) {
  c(lower = qlast_end_time(alpha/2,N, alpha_s, beta_s, alpha_d, beta_d), upper = qlast_end_time(1-alpha/2,N,alpha_s,beta_s,alpha_d, beta_d))
}

E_first_start_time = function(N, alpha_s, beta_s, lower=0, upper=1) {
  integrand <- function(x) {
          x * dfirst_start_time(x, N=N, alpha_s=alpha_s, beta_s=beta_s)
  }
  expected = integrate(integrand, lower, upper)$value
  return(c(expected,first_start_time_ci(N, alpha_s, beta_s,alpha=0.05)))
}

first_start_time_ci <- function(N, alpha_s, beta_s,alpha=0.05) {
  c(lower = qfirst_start_time(alpha/2,N, alpha_s, beta_s), upper = qfirst_start_time(1-alpha/2,N,alpha_s,beta_s))
}


expected_end_time = function(alpha_s, beta_s, alpha_d, beta_d) {
        mu_o = beta_mean(alpha_s, beta_s)
        mu_d = beta_mean(alpha_d, beta_d)
        return(mu_o + mu_d * (1 - mu_o))
}

expected_start_time = function(alpha_s, beta_s) {
        mu_o = beta_mean(alpha_s, beta_s)
        return(mu_o)
}

expected_duration = function(alpha_s, beta_s, alpha_d, beta_d) {
        mu_o = beta_mean(alpha_s, beta_s)
        mu_d = beta_mean(alpha_d, beta_d)
        return(mu_d * (1 - mu_o))
}

expected_observed = function(alpha_s, beta_s, alpha_d, beta_d) {
        mu_o = beta_mean(alpha_s, beta_s)
        mu_d = beta_mean(alpha_d, beta_d)
        return(mu_o + mu_d * (1 - mu_o)/2)
}


variance_start_time = function(alpha_s, beta_s) {
        var = beta_sd(alpha_s, beta_s)^2
        return(var)

}

variance_end_time = function(alpha_s, beta_s, alpha_d, beta_d, lower=0, upper=1) {
        eet = expected_end_time(alpha_s, beta_s, alpha_d, beta_d)
  integrand <- function(x) {
          x*x * dend_time(x, alpha_s=alpha_s, beta_s=beta_s, alpha_d=alpha_d, beta_d=beta_d)
  }
  eet2  = integrate(integrand, lower, upper)$value

  var = eet2 - eet^2
  if(var<0) { 
	  print(paste("Variance calculated to be less than 0 in variance_end_time. Returning 0. E[x^2] = ", eet2, " E[x]^2 = ",  eet^2))
	  return(0)
  }
  return(var)
}

#data are scaled!
#parameters original scale - easier gradient?
neg_loglik_observed_origScale = function(param, data,min=0, max=365) {
	d = max-min
	param[1] = (param[1] - min) / d
	param[2] = (param[2]) / d
	param[3] = (param[3]) / d
	param[4] = (param[4]) / d
	as =  beta_alpha(param[1], param[2])
	bs =  beta_beta(param[1], param[2])
	ad =  beta_alpha(param[3], param[4])
	bd =  beta_beta(param[3], param[4])
	return(neg_loglik_observed(param=c(as,bs,ad,bd), data=data))
}

neg_loglik_observed <- function(param, data) {
        return(-loglik_observed(param,data) )
}

# --- 3. Log-likelihood function for observed ---
loglik_observed <- function(param, data) {
  a_s <- param[1]; b_s <- param[2]
  a_d <- param[3]; b_d <- param[4]
  if (any(param <= 0)) return(1e10)
  dens <- dobserved(data, a_s, b_s, a_d, b_d)
  sum(log(dens + 1e-12))  # avoid log(0)
}

neg_log_posterior_origScale <- function(param, t_obs, a_o_m, b_o_m, a_d_m, b_d_m, a_o_sd, b_o_sd, a_d_sd, b_d_sd,min=0, max=365) {
	d = max-min
	param[1] = (param[1] - min) / d
	param[2] = (param[2]) / d
	param[3] = (param[3]) / d
	param[4] = (param[4]) / d
	as =  beta_alpha(param[1], param[2])
	bs =  beta_beta(param[1], param[2])
	ad =  beta_alpha(param[3], param[4])
	bd =  beta_beta(param[3], param[4])
	return(neg_log_posterior(params=c(as,bs,ad,bd), t_obs = t_obs, a_o_m, b_o_m, a_d_m, b_d_m, a_o_sd, b_o_sd, a_d_sd, b_d_sd))
}

neg_log_posterior <- function(params, t_obs, a_o_m, b_o_m, a_d_m, b_d_m, a_o_sd, b_o_sd, a_d_sd, b_d_sd) {
        #print("posterior eval")
  alpha_s <- params[1]
  beta_s <- params[2]
  alpha_d <- params[3]
  beta_d <- params[4]

  mu_o = beta_mean(alpha_s,beta_s)
  sd_o = beta_sd(alpha_s,beta_s)
  mu_d = beta_mean(alpha_d,beta_d)
  sd_d = beta_sd(alpha_d,beta_d)


  if (alpha_s <= 0 || beta_s <= 0 || sd_o <= 0 || sd_o > 0.25 ||
      alpha_d <= 0 || beta_d <= 0 || sd_d <= 0 || sd_d > 0.25) {
    print("Parameters are out of range during inference. Try setting priors")
    return(Inf)
  }

  # Log-likelihood
  log_lik <- loglik_observed(c(alpha_s,beta_s,alpha_d,beta_d),t_obs)

  # Log-prior
  log_prior <- dbeta(mu_o, a_o_m, b_o_m, log = TRUE) +
               dbeta(mu_d, a_d_m, b_d_m, log = TRUE) +
               dbeta(sd_o, a_o_sd, b_o_sd, log = TRUE) +
               dbeta(sd_d, a_d_sd, b_d_sd, log = TRUE)

  return(- (log_lik + log_prior))  # negative log-posterior
}





