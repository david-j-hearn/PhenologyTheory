library(cmdstanr)
library(bayesplot)
library(tidyverse)
library(posterior)
library(here)

source("phenologyDistributions.GP.R")

#overlap
	getProportionOverlap.OC.GP = function(mu_O, mu_C, sigma, min=0, max=365) {
		f <- function(x) dnorm(x, mu_O, sigma)
			g <- function(x) dnorm(x, mu_C, sigma)
			overlap <- integrate(function(x) pmin(f(x), g(x)), min,max)$value
			return(overlap)
	}
#peak
getPeak.T.GP = function(mu_O, mu_C) {
	peak = mu_O + (mu_C - mu_O) / 2
		return(peak)
}
#Expected:
#first start
E.Ok1.GP = Vectorize(function(N, mu_O, sigma, min=0, max=365, verbose=F, intFailLow=NA, intFailHigh=NA) {
		integrand <- function(x) { x * dOk1.GP(x, N=N, mu_O=mu_O, sigma=sigma) }
		if(verbose) {
		cat(paste("E.Ok1.GP: mu_O: ", mu_O, " sigma: ", sigma, " N: ", N, "\n"))
		}
		res = integrate(integrand, min, max, rel.tol = 1e-12, abs.tol = 1e-12, subdivisions = 10000L)
		if (res$message == "OK") {
		if(verbose) {
		cat("Integral =", res$value, "\n")
		cat("Estimated error =", res$abs.error, "\n")
		}
		} else {
		stop("Integration may not have converged. Message: ", res$message)
		return(NA)	#Never reached!
		}
		retVal = res$value
		if(!is.na(intFailLow)) {
			retVal[retVal < intFailLow] <- NA
		}
		if(!is.na(intFailHigh)) {
			retVal[retVal > intFailHigh] <- NA
		}

		return(retVal)
})
#start
E.O.GP = function(mu_O) {
	return(mu_O)
}
#observed
E.T.GP = function(mu_O, mu_D) {
	return(mu_O + mu_D/2)
}

#E.T.GP = function(mu_O, mu_C) {
#mu_D = mu_C - mu_O
#return(mu_O + mu_D/2)
#}
#cessation
E.C.GP = function(mu_C) {
	return(mu_C)
}
#last cessation
E.CkN.GP = Vectorize(function(N, mu_C, sigma, min=0, max=365, intFailLow=NA, intFailHigh=NA) {
		integrand <- function(x) { x * dCkN.GP(x, N=N, mu_C=mu_C, sigma=sigma) }
		expected = integrate(integrand, min, max)$value
		retVal = expected
		if(!is.na(intFailLow)) {
			retVal[retVal < intFailLow] <- NA
		}
		if(!is.na(intFailHigh)) {
			retVal[retVal > intFailHigh] <- NA
		}
		return(retVal)
		})
#duration
E.D.GP = function(mu_O, mu_C) {
	return(mu_C - mu_O)
}
#SD:
#first start
SD.Ok1.GP = function(N, mu_O, sigma, min=0, max=365, intFailLow=NA, intFailHigh=NA) {
	E = E.Ok1.GP(N, mu_O, sigma, min, max)
		integrand <- function(x) { x * x * dOk1.GP(x, N, mu_O, sigma) }
	E2 = integrate(integrand, min, max)$value
		retVal = E2
		if(!is.na(intFailLow)) {
			retVal[retVal < intFailLow] <- NA
		}
	if(!is.na(intFailHigh)) {
		retVal[retVal > intFailHigh] <- NA
	}
	E2 = retVal
		var = E2 - E^2
		if(var < 0) { stop("Error calculating variance for first onset times") }
	return(sqrt(var))
}
#start
SD.O.GP = function(sigma) {
	return(sigma)
}
#observed
SD.T.GP = function(mu_O, mu_C, sigma, min=0, max=365) {
	E = E.T.GP(mu_O, mu_C)
		integrand <- function(x) { x * x * dT.GP(x, mu_O, mu_C, sigma, min, max) }
	E2 = integrate(integrand, min, max)$value
		var = E2 - E^2
		if(var < 0) { stop("Error calculating variance for observed times") }
	return(sqrt(var))
}
#cessation
SD.C.GP = function(sigma) {
	return(sigma)
}
#last cessation
SD.CkN.GP = function(N, mu_C, sigma, min=0, max=365, intFailLow=NA, intFailHigh=NA) {
	E = E.CkN.GP(N, mu_C, sigma, min, max)
		integrand <- function(x) { x * x * dCkN.GP(x, N, mu_C, sigma) }
	E2 = integrate(integrand, min, max)$value
		retVal = E2
		if(!is.na(intFailLow)) {
			retVal[retVal < intFailLow] <- NA
		}
	if(!is.na(intFailHigh)) {
		retVal[retVal > intFailHigh] <- NA
	}
	E2 = retVal
		var = E2 - E^2
		if(var < 0) { stop("Error calculating variance for first onset times") }
	return(sqrt(var))
}
#duration
SD.D.GP = function() {
	warning("Duration has a Dirac delta distribution, so the variance is 0.")
		return(0)
}
#alpha probability interval (using quantile functions):
#first start
PI.Ok1.GP = function(N, mu_O, sigma, alpha=0.05) {
	lower = alpha/2
		upper = 1 - alpha/2
		return(qOk1.GP(c(lower, upper), N, mu_O, sigma))
}
#start
PI.O.GP = function(mu_O, sigma, alpha=0.05) {
	lower = alpha/2
		upper = 1 - alpha/2
		return(qO.GP(c(lower, upper), mu_O, sigma))
}
#observed
PI.T.GP = function(mu_O, mu_C, sigma, alpha=0.05) {
	lower = alpha/2
		upper = 1 - alpha/2
		return(qT.GP(c(lower, upper), mu_O, mu_C, sigma))
}
#cessation
PI.C.GP = function(mu_C, sigma, alpha=0.05) {
	lower = alpha/2
		upper = 1 - alpha/2
		return(qC.GP(c(lower, upper), mu_C, sigma))
}
#last cessation
PI.CkN.GP = function(N, mu_C, sigma, alpha=0.05) {
	lower = alpha/2
		upper = 1 - alpha/2
		return(qCkN.GP(c(lower, upper), N, mu_C, sigma))
}
#duration
PI.D.GP = function(mu_O, mu_C) {
	d = mu_C - mu_O
		warning("Duration has a Dirac delta distribution, so 100% of the samples fall at the duration")
		return(c(d,d))
}

runStan.NoCovariates.T.GP = function(fileOrData, min=0, max=365, N=NA, hyperparameters = c(100,7,50,7,10,7), dataProvided=F, runMAP=T, setStringent=F, processExtremes = F, maxDiv=0, ...) {
	options(mc.cores = 4)

		print("starting Stan run")
#get and scale observed times
		if(dataProvided) {
			observed = (fileOrData - min ) / (max - min)
		}
		else {
			observed = getObservations(fileOrData,min=min,max=max)
		}

	if(processExtremes && !is.na(N)) {
		processExtremes = 1
	}
	else {
		N = 0
			processExtremes = 0
	}

#set the hyperparameters
	mean_mean_onset = (hyperparameters[1] - min) / (max-min)
		sd_mean_onset = hyperparameters[2] / (max-min)
		mean_mean_duration = hyperparameters[3] / (max-min)
		sd_mean_duration = hyperparameters[4] / (max-min)
		mean_sd = hyperparameters[5] / (max-min) 		#taken just from the onset distribution sigma, as, in the GP model, duration is deterministic
		sd_sd = hyperparameters[6] / (max-min) 			#taken just from the onset distribution sigma, as, in the GP model,  duration is deterministic

#prepare data for Stan
		data <- list(
				N = length(observed),	#sample size
				n = N,			#population size for extremes
				t = observed,
				minResponse = min,
				maxResponse = max,
				debug = FALSE,
				drop_nc = FALSE,
				drop_ll = FALSE,
				processExtremes = processExtremes,
				mean_mean_onset = mean_mean_onset,
				sd_mean_onset = sd_mean_onset,
				mean_mean_duration = mean_mean_duration,
				sd_mean_duration = sd_mean_duration,
				mean_sd = mean_sd,
				sd_sd = sd_sd
			    )
		data$debug <- 0

#compile stan model
		print("Attempting to compile Stan model")
		m = tryCatch({
				cmdstan_model("noCovariates.gp.stan", stanc_options = list("allow-undefined"),  user_header = here::here("gp.hpp"))
				}, error = function(e) {
				ret = list(
						error = T,
						errorM = e$messageA
					  )
				return(ret)
				})


#sample the posterior based on the provided observations and hyperparameter values
	print("Attempting to sample Stan model")
		res = tryCatch({
				if(setStringent) {
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

	print("Checking divergences.")
	nDiv = sum(res$diagnostic_summary()$num_divergent)
	#print("OK!!")
		if(nDiv > maxDiv) {
			ret = list(
					error = T,
					errorM = paste("Stan run failed. ", nDiv, " divergences encountered.")
				  )
				return(ret)
		}


	if(processExtremes) {
		cat("Attempting to process phenological extremes.\n")
#for the draws, calculate the expected first onset and expected last cessation
			draws_df <- as_draws_df(res$draws())
			draws_df$E_CkN = E.CkN.GP(N = N, mu_C = draws_df$mu_C, sigma = draws_df$sigma, min=min, max=max)
			draws_df$E_Ok1 = E.Ok1.GP(N = N, mu_O = draws_df$mu_O, sigma = draws_df$sigma, min=min, max=max)
		#print(sort(draws_df$E_Ok1))
#cat("mu_O\n")
#print(sort(draws_df$mu_O))
#cat("EOk1\n")
#print(sort(draws_df$E_Ok1))
#there are integration errors going on!
#solution is to discard unreasonable values, but this may be biasing things... The onset values are great, so it is an R integration issue
#print("OK1")
#dx <- diff(draws_df$E_Ok1)
#print("OK2")
#threshold <- 10.0
#jump_indices <- which(dx > threshold)
#print("OK3")
#print(jump_indices)
##if(length(jump_indices)>0) { draws_df$E_Ok1 <- draws_df$E_Ok1[(max(jump_indices) + 1):length(draws_df$E_Ok1)] }
#print("OK3.1")
#print(jump_indices[1])
#length(jump_indices)
#print("OK3.2")
#if(length(jump_indices)>0) { draws_df$E_Ok1 = draws_df$E_Ok1[(jump_indices[1] + 1):length(draws_df$E_Ok1)] }
#print("OK4")
#print(sort(draws_df$E_Ok1))
	}
	else {
		draws_df = NA
	}

	if(runMAP) {
#get GP model MAP
		print("Attempting to optimize Stan model")
			resMAP = tryCatch({
					chain <- 1
					iter <- 999
					init_f_samp <- function() {
					init_df <- res$draws() %>% as_draws_df() %>% filter(.chain == chain, .iteration == iter)
					as.list(init_df)
					}
					m$optimize(data, init = init_f_samp)
					}, error = function(e) {
					ret = list(
							error = T,
							errorM = e$messageA
						  )
					return(ret)
					})

		print("done optimizing Stan model")
	}
	else { resMAP = NA }


	result = list(
			model = m, 				#Stan model
			sample = res,				#HMC sample
			draws_extremes = draws_df,		#NA if processExtremes is false
			MAP = resMAP,				#NA if runMap is false
			data = data,				#original data provided to Stan
			error = F,
			errorM = "Results were obtained, but check diagnostics of the sample."
		     )
		return(result)
}





#Handled automatically:
#scaling the observations between 0 and 1
#determination of the range in the covariate
#scaling and translating the covariate so that the min is 0 and the max is 1
#with the hyperparameter file information
#hyperparameter file provides for each covariate:
#mean and SD
#one covariate per line
runStan.WithCovariates.T.GP = function(response, min=0, max=365, onsetCovariates, durationCovariates, onsetHyperBeta, onsetHyperAnchor, durationHyperBeta, durationHyperAnchor, sigmaHyper=c(10,50), dataProvided=F, ...) {
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

	K_O = covariatesOnset$K
		K_D = covariatesDuration$K

#get the number of covariates for onset and for duration
		N = length(observed)
		N_O = nrow(covariatesOnset$scaledCovariates)
		N_D = nrow(covariatesDuration$scaledCovariates)


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

				min = min,
				max = max,

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
							cmdstan_model("withCovariates.gp.stan", stanc_options = list("allow-undefined"),  user_header = here::here("gp.hpp"))
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
				m$sample(stanData,...)
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
			data = stanData,
			error = F,
			error_m = "No error was detected during the Stan run"
		     )
		return(output)
}

