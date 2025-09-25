source("phenologyDistributions.R")

#wDifferentCovariates: covariates are different for onset and duration
simulate_observed_wDifferentCovariates_GP = function(min=0, max=365, replicates, covariateOnsetMin, covariateOnsetMax, covariateDurationMin, covariateDurationMax, alphaOnset, betaOnset, alphaDuration, betaDuration, sigma, maxReject=0.1) {

	ncolOnset = length(covariateOnsetMin)
	ncolDuration = length(covariateDurationMin)
	if(length(covariateOnsetMin) != ncolOnset || length(covariateOnsetMax) != ncolOnset || length(covariateDurationMin) != ncolDuration || length(covariateDurationMax) != ncolDuration || length(betaOnset) != ncolOnset || length(betaDuration) != ncolDuration) {
		return(list(
			error=T,
			error_m="Mismatched number of covariates. Be sure the length of the covariate min, covariate max, beta's vectors are the same for the onset parameters and the same for the duration parameters. There should be one value in each of these vectors for each covariate. There should be a single alpha for duration and a single alpha for onset."
		))
	}

	#set up data structures for simulated times
	t_start = rep(0,replicates)
	t_end = rep(0,replicates)
	observed = rep(0,replicates)

	#set up data structures for distribution parameters for each simulation
	mu_O = rep(0, replicates)
	mu_C = rep(0, replicates)
	D = rep(0, replicates)

	#set up data structures for simulated values of the covariates uniformly sampled in the range between covariate min and covariate max
	durationDesign = createZerosDataFrame(ncol = length(covariateDurationMin), nrow=replicates, colNamePrefix = "D")
	onsetDesign = createZerosDataFrame(ncol = length(covariateOnsetMin), nrow=replicates, colNamePrefix = "O")

	nRej = 0
	rep=1
	while(rep<=replicates) {
		durationDesign[rep,] = runif(ncolDuration,covariateDurationMin, covariateDurationMax)
		onsetDesign[rep,] = runif(ncolOnset,covariateOnsetMin, covariateOnsetMax)
		D[rep] = alphaDuration + as.matrix(durationDesign[rep,]) %*% betaDuration #dot product
		mu_O[rep] = alphaOnset + as.matrix(onsetDesign[rep,]) %*% betaOnset #dot product
		mu_C[rep] = D[rep] + mu_O[rep]
		t_start[rep] = rnorm(1,mu_O,sigma)
		t_end[rep] = t_start[rep] + D[rep]
		if(t_start[rep] < min || t_end[rep] > max) {
			nRej = nRej+1
			if(nRej > replicates*maxReject) {
				return(list(
					error=T,
					error_m="Input parameter values generated too many simulated values outside the acceptable range. Try other parameter value ranges"
				))
			}
			next
		}
		observed[rep] = runif(1, t_start[rep], t_end[rep])
		rep = rep + 1
	}

  return(list(
    error = F,
    error_m = paste("No errors detected during simulation with ", ncolOnset, " onset covariates and ", ncolDuration, " duration covariates."),
    n = replicates, 
    covariateDurationMin = covariateDurationMin, 
    covariateDurationMax = covariateDurationMax,
    covariateOnsetMin = covariateOnsetMin, 
    covariateOnsetMax = covariateOnsetMax,
    observed = observed,
    t_start = t_start,
    t_end = t_end,
    alphaOnset = alphaOnset,
    betaOnset = betaOnset,
    alphaDuration = alphaDuration,
    betaDuration = betaDuration,
    sigma = sigma,
    durationDesign = durationDesign,
    onsetDesign = onsetDesign,
    rejected = nRej
  ) )
}

#wSameCovariates: covariates are the same for onset and duration
simulate_observed_wSameCovariates_GP = function(min=0, max=365, replicates, covariateMin, covariateMax, alphaOnset, betaOnset, alphaDuration, betaDuration, sigma, maxReject=0.1) {

	ncol = length(covariateMin)
	if(length(covariateMin) != ncol || length(covariateMax) != ncol || length(betaOnset) != ncol || length(betaDuration) != ncol) {
		return(list(
			error=T,
			error_m="Mismatched number of covariates. Be sure the length of the covariateMin, covariateMax, beta's vectors are the same. There should be one value in each of these vectors for each covariate. There should be a single alpha for duration and a single alpha for onset."
		))
	}

	print(paste("Running simulation with onset slope ", betaOnset, " and duration slope ", betaDuration, " onset intercept ", alphaOnset, " and duration intercept " , alphaDuration))

	#set up data structures for simulated times
	t_start = rep(0,replicates)
	t_end = rep(0,replicates)
	observed = rep(0,replicates)

	#set up data structures for distribution parameters for each simulation
	mu_O = rep(0, replicates)
	mu_C = rep(0, replicates)
	D = rep(0, replicates)

	#set up data structures for simulated values of the covariates uniformly sampled in the range between covariateMin and covariateMax
	durationDesign = createZerosDataFrame(ncol = length(covariateMin), nrow=replicates, colNamePrefix = "D")
	onsetDesign = createZerosDataFrame(ncol = length(covariateMin), nrow=replicates, colNamePrefix = "O")

	nRej = 0
	rep=1
	while(rep<=replicates) {
		durationDesign[rep,] = runif(ncol,covariateMin, covariateMax)
		onsetDesign[rep,] = durationDesign[rep,]
		D[rep] = alphaDuration + as.matrix(durationDesign[rep,]) %*% betaDuration
		mu_O[rep] = alphaOnset + as.matrix(onsetDesign[rep,]) %*% betaOnset
		mu_C[rep] = D[rep] + mu_O[rep]
		t_start[rep] = rnorm(1,mu_O[rep],sigma)
		t_end[rep] = t_start[rep] + D[rep]
		if(t_start[rep] < min || t_end[rep] > max) {
			nRej = nRej+1
			if(nRej > replicates*maxReject) {
				return(list(
					error=T,
					error_m="Input parameter values generated too many simulated values outside the acceptable range. "
				))
			}
			next
		}
		observed[rep] = runif(1, t_start[rep], t_end[rep])
		rep = rep + 1
	}

  return(list(
    error = F,
    error_m = paste("No errors detected during simulation with ", ncol, " covariates."),
    n = replicates, 
    covariateMin = covariateMin, 
    covariateMax = covariateMax,
    observed = observed,
    t_start = t_start,
    t_end = t_end,
    durations = D,
    alphaOnset = alphaOnset,
    betaOnset = betaOnset,
    alphaDuration = alphaDuration,
    betaDuration = betaDuration,
    sigma = sigma,
    durationDesign = durationDesign,
    onsetDesign = onsetDesign,
    rejected = nRej
  ) )
}



#simulate data using beta onset and beta duration conditioned to onset + duration < (max - min). Observations randomly sampled between onset and cessation
#betaDuration means that the duration is the mean of the raw beta distribution for durations. This gets scaled to provide the true duration since the onset + duration must be less than max time
simulate_observed <- function(n, min=0, max=365, onset_mean, onset_sd, duration_mean, duration_sd, mins=1, maxs=3000, betaDuration=T, getExtremes=T) {
	if(!betaDuration) {
		print("The duration mean and SD should be provided in terms of the beta distribution that samples the raw durations. This will be scaled by (1 - onset_mean) to assure that duration + onset falls between min and max times. Take your duration_mean that you want to simulate and divide it by (1 - (onset_mean-min)/(max-min)) to get the duration to input here. Note that the duration SD needs to be scaled as well, but its scaling is more complicated. Therefore, provide an estimate of your duration SD for the beta distribution.")

	  return(list(error = T,
		      error_m = "Beta durations must be used. The alternative is not yet implemented. See message printed when you ran this function with betaDuration=F"))
	}

  #scale to beta distribution support
  onset_mean = (onset_mean-min)/(max-min)
  onset_sd = onset_sd / (max - min)
  duration_mean = duration_mean / (max - min)
  duration_sd = duration_sd / (max - min)

  # Convert mean/sd to alpha/beta for onset
  alpha_s = beta_alpha(onset_mean, onset_sd)
  beta_s = beta_beta(onset_mean, onset_sd)

  # Convert mean/sd to alpha/beta for duration
  alpha_d = beta_alpha(duration_mean, duration_sd)
  beta_d = beta_beta(duration_mean, duration_sd)

  #print(paste("t_start <- rbeta(", n, ",  ", alpha_s, ",", beta_s, ")"))

  if(alpha_s<mins || alpha_s>maxs || alpha_d<mins || alpha_d>maxs || beta_s<mins || beta_s>maxs || beta_d<mins || beta_d>maxs) {
	  print(c(alpha_s,beta_s,alpha_d,beta_d))
	  return(list(error_m = "Infeasible parameter value provided as input under beta cessation, beta duration model", error=T))

  }

  # Simulate onset times
  t_start <- rbeta(n, alpha_s, beta_s)

  # Simulate durations scaled to (0, 1 - onset)
  raw_duration <- rbeta(n, alpha_d, beta_d)
  duration <- raw_duration * (1 - t_start)

  t_end <- t_start + duration

  t_start = min + t_start * (max-min)
  t_end = min + t_end * (max-min)

  # Uniform sample between start and end for each individual
  observed <- runif(n, min = t_start, max = t_end)

  if(getExtremes) {
  failed = F
  #these are messed up! This is not the right way to do this, but it works
  cessation_sd = tryCatch( {
	  variance = variance_end_time(alpha_s,beta_s,alpha_d,beta_d) #this is costly time-wise
	  if(variance <=0) { 
		  failed = T
		  return(list(error = T, 
			      error_m = "Failed to obtain cessation variance under beta onset, beta duration model"))
	  }
	  sqrt(variance) * (max - min) #this is costly time-wise
  }, error = function(e) {
		  failed = T
	  return(list(error = T,
		      error_m = "Failed to obtain cessation variance under beta onset, beta duration model"))
  })
    e_last_end_time = tryCatch({
	    min + E_last_end_time(n, alpha_s, beta_s, alpha_d, beta_d) * (max-min)
    }, error = function(e) {
		  failed = T
	  return(list(error = T,
		      error_m = "Failed to obtain last cessation time under beta onset, beta duration model."))
  })

    e_first_start_time = tryCatch( {
	    min + E_first_start_time(n, alpha_s, beta_s) * (max-min)
    }, error = function(e) {
		  failed = T
	  return(list(error = T,
		      error_m = "Failed to obtain first onset time under beta onset, beta duration model."))

    })

    if(failed) {
	  return(list(error = T,
		      error_m = "Simulation failed under beta onset, beta duration model."))
    }
  }
  else {
  	cessation_sd = NA
	e_last_end_time = c(NA,NA,NA)
	e_first_start_time = c(NA,NA,NA)
  }


  return(list(
    error = F,
    error_m = "No errors detected during simulation under beta onset, beta duration model.",
    n = n, 
    min = min, 
    max = max,
    observed = observed,
    t_start = t_start,
    t_end = t_end,
    alpha_s = alpha_s,
    beta_s = beta_s,
    alpha_d = alpha_d,
    beta_d = beta_d,
    onset_mean = min + onset_mean * (max - min), 
    onset_sd = onset_sd * (max - min), 
    duration_mean_raw = duration_mean * (max - min), #for raw duration
    duration_sd_raw = duration_sd * (max - min), #for raw duration
    duration_mean = duration_mean * (1 - onset_mean) * (max - min), #infers the beta parameters for the raw distribution, here is the scaled result for the duration
    cessation_mean = min + (onset_mean + duration_mean * ( 1 - onset_mean)) * (max - min),
    cessation_sd = cessation_sd,
    last_end_time = max(t_end),
    first_start_time = min(t_start),
    e_last_end_time = e_last_end_time,
    e_first_start_time = e_first_start_time
  ) )
}

#simulate_observed_GP = function(n, mu_O, mu_C, sd, min=0, max=1) {
#
	#mu_O = (mu_O-min) / (max - min)
	#mu_C = (mu_C-min) / (max - min)
	#if(mu_O>=mu_C) {
		#print("The mean onset must be before the mean cessation. Quitting.")
		#return()
	#}
	#tot = 1
	#out = rep(0,n)
	#while( tot <= n ) {
		#tempU = runif(1)
		#tempN = rnorm(1,tempU,sd)
		#if(tempN > mu_O && tempN < mu_C) {
			#out[tot] = tempU
			#tot=tot+1
		#}
	#}
	#return(out)
#}

#simulate_observed_GP_M3 = function(n, mu_O, mu_C, sd) {
	#if(mu_O>=mu_C) {
		#print("The mean onset must be before the mean cessation. Quitting.")
		#return()
	#}
	#d = mu_C - mu_O
	#start = rnorm(n,mu_O,sd)
	#end = start + d
	#obs = runif(n,start,end)
#
    	#e_last_end_time = tryCatch( {
		#E_last_end_time_GP(n, mu_C, sigma=sd)
	#}, error = function(e) { 
	  #return(list(error = T,
		      #error_m = "Failed to obtain last cessation time under GP model under beta cessation, beta duration model."))
	#})
#
    	#e_first_start_time = tryCatch( {
		#E_first_start_time_GP(n, mu_O, sigma=sd)
	#}, error = function(e) {
	  #return(list(error = T,
		      #error_m = "Failed to obtain first onset time under GP model under beta cessation, beta duration model."))
	#})
#
	#output = list( 
    #error = F,
    #error_m = "No errors detected during simulation under beta cessation, beta duration model.",
    #n = n,
    #observed =  obs,
    #observed_mean = expected_observed_GP(mu_O, mu_C),
    #t_start = start,
    #t_end = end,
    #onset_mean = mu_O,
    #duration_mean = d,
    #cessation_mean = mu_O + d,
    #sigma = sd,
    #last_end_time = max(end),
    #first_start_time = min(start),
    #e_last_end_time = e_last_end_time,
    #e_first_start_time = e_first_start_time
  #) 
#
	#return(output)
#}

simulate_observed_GP_M2 = function(n, mu_O, mu_C, sd, min=0, max=1) {
	mu_O = (mu_O-min) / (max - min)
	mu_C = (mu_C-min) / (max - min)
	sd = sd / (max - min)
	if(mu_O>=mu_C) {
		print("The mean onset must be before the mean cessation. Quitting.")
		return()
	}
	d = mu_C - mu_O
	start = rnorm(n,mu_O,sd)
	start = start[start>0]
	start = start[start+d<1]
	tot = length(start)
	nRej = 0
	while(tot<n) {
		nRej = nRej + (n-tot)
		temp = rnorm(n-tot,mu_O,sd)
		temp = temp[temp>0]
		temp = temp[temp+d<1]
		start = c(start,temp)
		tot = tot + length(temp)
	}
	if(nRej>0.1*n) {
		print(paste("Warning: rejected start times below 0 ", nRej, " times. This can bias inferences. If ", nRej, "is less than, say, 0.1% of the sample size (", (n*0.01), " in your case), this should be ok, otherwise, the model inference will be highly biased"))
		return(list(error=T))
	}
	end = start + d
	obs = runif(n,start,end)

    	e_last_end_time = tryCatch( {
		min + E_last_end_time_GP(n, mu_C, sigma=sd) * (max-min)
	}, error = function(e) { 
	  return(list(error = T,
		      error_m = "Failed to obtain last cessation time under GP model under beta cessation, beta duration model."))
	})

    	e_first_start_time = tryCatch( {
		min + E_first_start_time_GP(n, mu_O, sigma=sd) * (max-min)
	}, error = function(e) {
	  return(list(error = T,
		      error_m = "Failed to obtain first onset time under GP model under beta cessation, beta duration model."))
	})

	output = list( 
    error = F,
    error_m = "No errors detected during simulation under beta cessation, beta duration model.",
    n = n,
    min = min,
    max = max,
    observed = min + obs*(max-min),
    observed_mean = min + expected_observed_GP(mu_O, mu_C) * (max-min),
    t_start = min + start * (max-min),
    t_end = min + end * (max-min),
    onset_mean = min + mu_O * (max - min),
    onset_sd = sigma,
    duration_mean = d * (max - min),
    cessation_mean = min + (mu_O + d) * (max - min),
    cessation_sd = sd * (max - min),
    sigma = sd * (max - min),
    last_end_time = min + max(end) * (max - min),
    first_start_time = min + min(start) * (max - min),
    e_last_end_time = e_last_end_time,
    e_first_start_time = e_first_start_time,
    rejected = nRej
  ) 

	return(output)
}

simulate_phenophaseRanges_GP = function(reps, N, mu_O, mu_C, sd, min=0, max=365) {
	r = rep(0,reps)
	for(i in 1:reps) {
		sim = simulate_observed_GP_M2(n = N, mu_O=mu_O, mu_C=mu_C, sd=sd, min=min, max=max) 
		if(sim$rejected > 0) { warning("The theoretical distribution of ranges is unlikely to match this simulated set of ranges due rejection of simulated values during sampling.") }
		r[i] = sim$last_end_time - sim$first_start_time
	}
	return(r)
}

simulate_phenophaseRanges = function(reps, N, mu_O, mu_D, sd_O, sd_D, min=0, max=365) {
	r = rep(0,reps)
	for(i in 1:reps) {
		sim = simulate_observed(n = N, onset_mean=mu_O, onset_sd=sd_O, duration_mean=mu_D, duration_sd=sd_D,getExtremes=F, min=min, max=max) 
		r[i] = sim$last_end_time - sim$first_start_time
	}
	return(r)
}
