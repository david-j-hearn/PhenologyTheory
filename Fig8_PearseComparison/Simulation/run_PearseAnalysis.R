source("phenologyInference.GP.R")
source("phenologyInference.R")
source("phenologyDistributions.GP.R")
source("phenologySimulation.R")
library(phest)

run_PearseAnalysis = function(replicates=1000, outputFile="output.txt", seed=12345, append=F) {

getInput = T
maxDiv = 0
maxRej = 5

#numerical integration error thresholds
#intFailLow=10
#intFailHigh=150

#intFailLow_Ok1q025=5
#intFailHigh_Ok1q975=150

#intFailLow_Oq025 = 50		#happens about 1 out of 10000
#intFailHigh_Oq975 = 250 		#happens about 1 out of 10000

#intFailLow_O = 85		#happens about 1 out of 10000
#intFailHigh_O = 200		#happens about 1 out of 10000

threshApprox = 2		#if the numerical integration value is off by more than threshApprox, the asymtotic approximation of the extreme is used

#response variable (day of year) 
min = 0
max = 365
d = max - min

#sub samples sizes for pearse and stan estimation
minSS=10
maxSS=90
SS = seq(minSS, maxSS, by=2)

#set up Pearse parameters
maxK = 90

#pfe_f_o = NA
#plci_f_o = NA
#puci_f_o = NA

#pfe_s_o = NA
#plci_s_o = NA
#puci_s_o = -888.0

#population sizes considered
N = c(100,10000,1000000)

StanEarliestEst = rep(0, 3)
e_first_start_time = rep(0, 3)

#set parameters
meanO = (150 - min) / d
meanD = 30 / d
meanC = meanO + meanD
sigma = 10 / d

#set up hyperparameter means
maxBias_mO = 21 / d         #must be less than or equal to min_mO
maxPrecision_mO = 14 / d
minPrecision_mO = 21 / d

maxBias_mD = 14 / d
maxPrecision_mD = 7 / d
minPrecision_mD = 14 / d

maxBias_sigma = 7 / d
maxPrecision_sigma = 7 / d
minPrecision_sigma = 14 / d


cat(sprintf("AP_Replicate\tAP_PopulationSize\tAP_SampleSize\tNumber_Divergences\tNumber_Runs_Rejected\tTrueParam_MeanOnset\tTrueParam_MeanDuration\tTrueParam_MeanCessation\tTrueParam_Sigma\tTrueObserved_FirstOnset\tTheorParam_ExpectedFirstOnset_fromTrueModel\tTheorParam_q2.5FirstOnset_fromTrueModel\tTheorParam_q97.5FirstOnset_fromTrueModel\tTrueHyper_mean_MeanOnset\tTrueHyper_sd_MeanOnset\tTrueHyper_mean_MeanDuration\tTrueHyper_sd_MeanDuration\tTrueHyper_mean_Sigma\tTrueHyper_sd_Sigma\tEst_Stan_HMC_MeanOnset\tEst_Stan_HMC_MeanDuration\tEst_Stan_HMC_MeanCessation\tEst_Stan_HMC_Sigma\tEst_Stan_HMC_q2.5_MeanOnset\tEst_Stan_HMC_q2.5_MeanDuration\tEst_Stan_HMC_q2.5_MeanCessation\tEst_Stan_HMC_q2.5_Sigma\tEst_Stan_HMC_q97.5_MeanOnset\tEst_Stan_HMC_q97.5_MeanDuration\tEst_Stan_HMC_q97.5_MeanCessation\tEst_Stan_HMC_q97.5_Sigma\tEstTheor_Stan_HMC_FirstOnset\tEstTheor_Stan_HMC_q2.5_FirstOnset\tEstTheor_Stan_HMC_q97.5_FirstOnset\tEst_Pearse_FullSample_FirstOnset\tEst_Pearse_FullSample_q2.5_FirstOnset\tEst_Pearse_FullSample_q97.5_FirstOnset\tEst_Pearse_SubSample_FirstOnset\tEst_Pearse_SubSample_q2.5_FirstOnset\tEst_Pearse_SubSample_q97.5_FirstOnset\tEst_Pearse_FullSample_FirstOnset_onset\tEst_Pearse_FullSample_q2.5_FirstOnset_onset\tEst_Pearse_FullSample_q97.5_FirstOnset_onset\tEst_Pearse_SubSample_FirstOnset_onset\tEst_Pearse_SubSample_q2.5_FirstOnset_onset\tEst_Pearse_SubSample_q97.5_FirstOnset_onset\n"),file=outputFile,append=append)

ir=1
nRej=0
while(ir <= replicates) {
	r = ir
	jn=1
	while(jn <= length(N)) {
		n=N[jn]
		kss=1
		#minI = min
		#maxI = meanO*d + min + sigma*d
		#E.Ok1.GP = Vectorize(function(N, mu_O, sigma, min=0, max=365, verbose=F, intFailLow=NA, intFailHigh=NA)
		e_first_start_time[1] = E.Ok1.GP(N=n, mu_O=meanO*d + min, sigma = sigma*d, min=min, max=max) 
		expEarliest = e_first_start_time[1]
		while(kss <= length(SS)) {
			ss = SS[kss]

cat("simulating data\n")
			#simulate data
			#sim = simulate_observed_GP_M2(n=n, min=min, max=max, mu_O = meanO * d + min, mu_C = (meanO + meanD)*d + min, sd = sigma * d, getExtremes=F)
#simulatePopulation =  function(N, mu_O, sigma_O, mu_D_raw, sigma_D=NA, minResponse=0, maxResponse=365, mins=1.5, maxs=3000, type="GP")
			sim = simulatePopulation(N=n, mu_O=meanO * d + min, sigma_O = sigma * d, mu_D_raw = meanD*d, minResponse=min, maxResponse=max, type="GP")

print(sim)

			#trueEarliest = min(sim$t_start)
			trueEarliest = min(sim$O)

			#set up parameters for Pearse
			k = ifelse(maxK<=ss, maxK, ss)

cat("running pearse full analysis\n")
			#analyze data using the complete sample from the population
		        pearse_full = tryCatch( {
                	#weib.limit(x=sim$observed, k=k, upper=F, alpha=0.05)
                	weib.limit(x=sim$Ts, k=k, upper=F, alpha=0.05)
        			}, error = function(e) {
                			c(F)
        		})
			if(is.na(pearse_full[1])) {
                		pfe_f = NA
                		plci_f = NA
                		puci_f = NA
			}
			else if(is.nan(pearse_full[1])) {
                		pfe_f = NA
                		plci_f = NA
                		puci_f = NA
			}
        		else if(!pearse_full[1]) {
                		pfe_f = NA
                		plci_f = NA
                		puci_f = NA
        		}
        		else {
                		pfe_f = pearse_full[1]
                		plci_f = pearse_full[2]
                		puci_f = pearse_full[3]
        		}


cat("running pearse sub analysis\n")
			#analyze data using a smaller sample from the population
			#subSample = sample(sim$observed,ss,replace=F)
			subSample = sample(sim$Ts,ss,replace=F)

			#run pearse on the sub sample
		        pearse_sub = tryCatch( {
                		weib.limit(x=subSample, k=k, upper=F, alpha=0.05)
        		}, error = function(e) {
                		c(F)
        		})
			if(is.na(pearse_sub[1])) {
                		pfe_s = NA
                		plci_s = NA
                		puci_s = NA
			}
			else if(is.nan(pearse_sub[1])) {
                		pfe_s = NA
                		plci_s = NA
                		puci_s = NA
			}
        		else if(!pearse_sub[1]) {
                		pfe_s = NA
                		plci_s = NA
                		puci_s = NA
        		}
        		else {
                		pfe_s = pearse_sub[1]
                		plci_s = pearse_sub[2]
                		puci_s = pearse_sub[3]
        		}

			##analyze data using the complete set of onset values from the population
cat("Running Pearse analysis with full onset data.\n")
		        pearse_full = tryCatch( {
                	weib.limit(x=sim$O, k=k, upper=F, alpha=0.05)
        			}, error = function(e) {
                			c(F)
        		})
			if(is.na(pearse_full[1])) {
                		pfe_f_o = NA
                		plci_f_o = NA
                		puci_f_o = NA
			}
			else if(is.nan(pearse_full[1])) {
                		pfe_f_o = NA
                		plci_f_o = NA
                		puci_f_o = NA
			}
        		else if(!pearse_full[1]) {
                		pfe_f_o = NA
                		plci_f_o = NA
                		puci_f_o = NA
        		}
        		else {
                		pfe_f_o = pearse_full[1]
                		plci_f_o = pearse_full[2]
                		puci_f_o = pearse_full[3]
        		}

			#analyze data using a smaller sample from the population
cat("Running Pearse analysis with a subsample of onset data.\n")
			subSample_o = sample(sim$O,ss,replace=F)

			#run pearse on the sub sample
		        pearse_sub = tryCatch( {
                		weib.limit(x=subSample_o, k=k, upper=F, alpha=0.05)
        		}, error = function(e) {
                		c(F)
        		})
			if(is.na(pearse_sub[1])) {
                		pfe_s_o = NA
                		plci_s_o = NA
                		puci_s_o = NA
			}
			else if(is.nan(pearse_sub[1])) {
                		pfe_s_o = NA
                		plci_s_o = NA
                		puci_s_o = NA
			}
        		else if(!pearse_sub[1]) {
                		pfe_s_o = NA
                		plci_s_o = NA
                		puci_s_o = NA
        		}
        		else {
                		pfe_s_o = pearse_sub[1]
                		plci_s_o = pearse_sub[2]
                		puci_s_o = pearse_sub[3]
        		}

			#set up hyperparameters for Stan analysis
			hMean_mO = runif(1,meanO - maxBias_mO, meanO + maxBias_mO) * d + min
			hSigma_mO = runif(1,maxPrecision_mO, minPrecision_mO) * d       

			hMean_mD = runif(1,meanD - maxBias_mD, meanD + maxBias_mD) * d
			hSigma_mD = runif(1,maxPrecision_mD, minPrecision_mD) * d
			
			hMean_sigma = runif(1,sigma - maxBias_sigma, sigma + maxBias_sigma) * d
			hSigma_sigma = runif(1,maxPrecision_sigma, minPrecision_sigma) * d

			hyperparameters_Stan = c(hMean_mO,hSigma_mO,hMean_mD,hSigma_mD,hMean_sigma,hSigma_sigma)

			#run Stan analysis
			#print(paste(r, ": Running optimization on subsampled data: Stan"))
			resultStan = tryCatch({
				#runUnivariateGPPhenology(subSample, min=min, max=max, hyperparameters = hyperparameters_Stan, dataProvided=T, runMAP=F, nRej=nRej)
				if(nRej > maxRej) {
					cat("!!! Too many rejections. Setting stringent mode. \n\n")
					runStan.NoCovariates.T.GP(fileOrData=subSample, minResponse=min, maxResponse=max, N=n, hyperparameters = hyperparameters_Stan, dataProvided=TRUE, runMAP=FALSE, setStringent=TRUE, processExtremes=TRUE, maxDiv=maxDiv, threshApprox=threshApprox)
					}
				else {
					print("Here")
					print(min)
					print(max)
					print(hyperparameters_Stan)

#runStan.NoCovariates.T.GP = function(fileOrData, min=0, max=365, N=NA, hyperparameters = c(100,7,50,7,10,7), dataProvided=F, runMAP=T, setStringent=F, processExtremes = F, maxDiv=0, ...) 


					runStan.NoCovariates.T.GP(fileOrData=subSample, min=min, max=max, N=n, hyperparameters = hyperparameters_Stan, dataProvided=TRUE, runMAP=FALSE, setStringent=FALSE, processExtremes=TRUE, maxDiv=maxDiv, threshApprox=threshApprox)
				}
			}, error = function(e) {
				message <- conditionMessage(e)
				print(message)
				print("stan error")
				list(error=TRUE)
				stop()
			})
			if(resultStan$error) {
				cat(paste("Stan sampling failed at replicate ", r, " with error:\n", resultStan$errorM, "\n"))
			nRej = nRej + 1
			#stop("eek!")
			next
			}

			#this is now handled in the runStan function!
			nDiv = sum(resultStan$sample$diagnostic_summary()$num_divergent)
			if(nDiv > maxDiv) {
				nRej = nRej + 1
				cat(paste("Rejected: ", nRej, " Too many divergences. Trying replicate again.", "\n"))
				next
			}

			#summary = resultStan$sample$summary(variables=c("mu_O","mu_D","mu_C","sigma", "E_Ok1", "E_CkN"),posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()
			summary = resultStan$sample$summary(variables=c("mu_O","mu_D","mu_C","sigma"),posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()

			E_Ok1 = resultStan$draws_extremes$E_Ok1
#print("from runStan")
#print(E_Ok1)
                	#E_Ok1[E_Ok1 < intFailLow] = NA
#print("first filter")
#print(E_Ok1)
                	#E_Ok1[E_Ok1 > intFailHigh] = NA
#print("second filter")
#print(E_Ok1)

                	#dx <- diff(E_Ok1)
                	#threshold <- 6
                	#jump_indices <- which(dx > threshold)
#print("OK1")
                	#if(length(jump_indices)>0) { 
#print("OK1.1")
				#last_jump <- jump_indices[length(jump_indices)]
				#E_Ok1 = E_Ok1[(last_jump + 1):length(E_Ok1)] 
			#}
			E_Ok1 = na.omit(E_Ok1)
			print("Length started at 4000. Now it is ")
			print(length(E_Ok1))
			
#print("OK2")
                	#print(sort(E_Ok1))

			est_mO_Stan = summary$mean[summary$variable=="mu_O"]
			est_mD_Stan = summary$mean[summary$variable=="mu_D"]
			est_mC_Stan = summary$mean[summary$variable=="mu_C"]
			est_SD_Stan = summary$mean[summary$variable=="sigma"]
			StanEarliestEst[1] = mean(E_Ok1)
			
			est_mO_Stan_q2.5 = summary$q2.5[summary$variable=="mu_O"]
			est_mD_Stan_q2.5 = summary$q2.5[summary$variable=="mu_D"]
			est_mC_Stan_q2.5 = summary$q2.5[summary$variable=="mu_C"]
			est_SD_Stan_q2.5 = summary$q2.5[summary$variable=="sigma"]
			StanEarliestEst[2] = quantile(E_Ok1, probs = 0.025)

			est_mO_Stan_q97.5 = summary$q97.5[summary$variable=="mu_O"]
			est_mD_Stan_q97.5 = summary$q97.5[summary$variable=="mu_D"]
			est_mC_Stan_q97.5 = summary$q97.5[summary$variable=="mu_C"]
			est_SD_Stan_q97.5 = summary$q97.5[summary$variable=="sigma"]
			StanEarliestEst[3] = quantile(E_Ok1, probs = 0.975)

			#check numerical integration failures - now handled in the routines that calculate the expectations of the extremes
			#if(!is.na(est_mO_Stan)) {
				#if(est_mO_Stan < intFailLow_O) { 
					#warning(paste("The estimate of the mean onset of ", est_mO_Stan, " fell below the threshold of ", intFailLow_O))
					#est_mO_Stan = NA 
				#}
				#else if(est_mO_Stan > intFailHigh_O) { 
					#warning(paste("The estimate of the mean onset of ", est_mO_Stan, " fell above the threshold of ", intFailHigh_O))
					#est_mO_Stan = NA 
				#}
			#}
			#if(!is.na(est_mO_Stan_q2.5) && est_mO_Stan_q2.5 < intFailLow_Oq025) { 
				#warning(paste("The estimate of the q2.5 of mean onset of ", est_mO_Stan_q2.5, " fell below the threshold of ", intFailLow_O_q025))
				#est_mO_Stan_q2.5 = NA 
			#}
			#if(!is.na(est_mO_Stan_q97.5) && est_mO_Stan_q97.5 > intFailHigh_Oq975) { 
				#warning(paste("The estimate of the q97.5 of mean onset of ", est_mO_Stan_q97.5, " fell above the threshold of ", intFailLow_O_q975))
				#est_mO_Stan_q97.5 = NA 
			#}

			#if(!is.na(StanEarliestEst[2]) && StanEarliestEst[2] < intFailLow_Ok1q025) { 
				#print(paste("rejecting q2.5 first onset: ", StanEarliestEst[2]))
				#StanEarliestEst[2] = NA 
				#}
			#if(!is.na(StanEarliestEst[3]) && StanEarliestEst[3] > intFailHigh_Ok1q975) { 
				#print(paste("rejecting q97.5 first onset: ", StanEarliestEst[3]))
				#StanEarliestEst[3] = NA 
				#}
			#if(is.na(StanEarliestEst[2]) || is.na(StanEarliestEst[3])) {
				#StanEarliestEst[1]=NA
			#}

			#print the results

			cat("\n\n!!!\n")
			cat(paste("Replicate: ", r, " Total Stan runs rejected: ", nRej, " Population size: ", n, " Sample size: ", ss, "\n"))
			cat(paste(ir, " (of ", replicates, ")", jn, " (of ", length(N), ") ", kss, " (of ", length(SS), ") ", "\n"))
			cat(paste("The true earliest is ", trueEarliest, "\n"))
			cat(paste("The expected earliest is ", expEarliest, "\n"))
			print(expEarliest)
                	cat(paste("Pearse full observed data: ", pfe_f, "(", plci_f, ",", puci_f, ")", "\n"))
                	cat(paste("Pearse subsampled observed data: ", pfe_s, "(", plci_s, ",", puci_s, ")", "\n"))
                	cat(paste("Pearse full onset data: ", pfe_f_o, "(", plci_f_o, ",", puci_f_o, ")", "\n"))
                	cat(paste("Pearse subsampled onset data: ", pfe_s_o, "(", plci_s_o, ",", puci_s_o, ")", "\n"))
			cat(paste("The onset estimates are: ", est_mO_Stan , "(", est_mO_Stan_q2.5, ",", est_mO_Stan_q97.5, ")", "\n"))
                	cat(paste("Stan subsampled data: ", StanEarliestEst[1], "(", StanEarliestEst[2], ",", StanEarliestEst[3], ")", "\n"))
			cat("!!!\n\n")

			if(getInput) {
				test = readline(prompt = "Type done to continue without further interruption.")
				if(test == "done") { getInput = FALSE }
			}


			#cat(paste(
					#r, "\n",
					#n, "\n",
					#ss, "\n",
					#nDiv, "\n",
					#nRej, "\n",
					#min + meanO * d, "\n",
					#meanD * d, "\n",
					#min + meanC * d, "\n",
					#sigma * d, "\n",
					#trueEarliest, "\n",
					#e_first_start_time[1], "\n",
					#e_first_start_time[2], "\n",
					#e_first_start_time[3], "\n",
					#hMean_mO, "\n",
					#hSigma_mO, "\n",
					#hMean_mD, "\n",
					#hSigma_mD, "\n",
					#hMean_sigma, "\n",
					#hSigma_sigma, "\n",
					#est_mO_Stan, "\n",
					#est_mD_Stan, "\n",
					#est_mC_Stan, "\n",
					#est_SD_Stan, "\n",
					#est_mO_Stan_q2.5, "\n",
					#est_mD_Stan_q2.5, "\n",
					#est_mC_Stan_q2.5, "\n",
					#est_SD_Stan_q2.5, "\n",
					#est_mO_Stan_q97.5, "\n",
					#est_mD_Stan_q97.5, "\n",
					#est_mC_Stan_q97.5, "\n",
					#est_SD_Stan_q97.5, "\n",
					#StanEarliestEst[1], "\n",
					#StanEarliestEst[2], "\n",
					#StanEarliestEst[3], "\n",
					#pfe_f, "\n",
					#plci_f, "\n",
					#puci_f, "\n",
					#pfe_s, "\n",
					#plci_s, "\n",
					#puci_s, "\n",
					#pfe_f_o, "\n",
					#plci_f_o, "\n",
					#puci_f_o, "\n",
					#pfe_s_o, "\n",
					#plci_s_o, "\n",
					#puci_s_o, "\n"))

			pRes = tryCatch({
				cat(sprintf("%d\t%d\t%d\t%d\t%d\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\t%.5f\n",
					r,
					n,
					ss,
					nDiv,
					nRej,
					min + meanO * d,
					meanD * d,
					min + meanC * d,
					sigma * d,
					trueEarliest,
					e_first_start_time[1],
					NA,
					NA,
					hMean_mO,
					hSigma_mO,
					hMean_mD,
					hSigma_mD,
					hMean_sigma,
					hSigma_sigma,
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
					pfe_f,
					plci_f,
					puci_f,
					pfe_s,
					plci_s,
					puci_s,
					pfe_f_o,
					plci_f_o,
					puci_f_o,
					pfe_s_o,
					plci_s_o,
					puci_s_o
					),file=outputFile, append=T)
        			}, error = function(e) {
					message <- conditionMessage(e)
					cat(paste("Print statement failed. Retrying. Error: ", message, "\n"))
					stop("Failed to print results. Quitting.")
			})	#print try / catch
		kss = kss+1
		nRej=0
		}		#sample size
	jn = jn+1
	}			#population size
ir = ir+1
}				#replicates


}				#function

