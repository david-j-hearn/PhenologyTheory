extractVariables = function(stanRunResult, meanCovariates, minCovariates, maxCovariates, responseMin, responseMax) {

range = responseMax - responseMin
rangeCovariates = maxCovariates - minCovariates

betaSummaryO = stanRunResult$summary(variables="betaO",posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()
betaSummaryD = stanRunResult$summary(variables="betaD",posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()

betasO_raw = betaSummaryO$mean
betasO_raw_q2.5 = betaSummaryO$q2.5
betasO_raw_q97.5 = betaSummaryO$q97.5

betasD_raw = betaSummaryD$mean
betasD_raw_q2.5 = betaSummaryD$q2.5
betasD_raw_q97.5 = betaSummaryD$q97.5

betasO = betasO_raw * range / rangeCovariates
betasO_q2.5 = betasO_raw_q2.5 * range / rangeCovariates
betasO_q97.5 = betasO_raw_q97.5 * range / rangeCovariates

betasD = betasD_raw * range / rangeCovariates
betasD_q2.5 = betasD_raw_q2.5 * range / rangeCovariates
betasD_q97.5 = betasD_raw_q97.5 * range / rangeCovariates

summary = stanRunResult$summary(variables = c("anchorO", "anchorD", "alphaO", "alphaD", "sigma"),posterior::default_summary_measures()[1:4],quantiles = ~ quantile2(., probs = c(0.025, 0.975)),posterior::default_convergence_measures()) %>% as.data.frame()

anchorOnset = responseMin + summary[summary$variable=="anchorO",]$mean * range
anchorOnset_q2.5 = responseMin + summary[summary$variable=="anchorO",]$q2.5 * range
anchorOnset_q97.5 = responseMin + summary[summary$variable=="anchorO",]$q97.5 * range

anchorDuration = summary[summary$variable=="anchorD",]$mean * range
anchorDuration_q2.5 = summary[summary$variable=="anchorD",]$q2.5 * range
anchorDuration_q97.5 = summary[summary$variable=="anchorD",]$q97.5 * range

#alphaO = anchorOnset - sum(betasO * meanCovariates)	#doesn't include 95%CI
#alphaD = anchorDuration - sum(betasD * meanCovariates)  #doesn't include 95%CI

alphaO = summary[summary$variable=="alphaO",]$mean * range + responseMin - sum(betasO * meanCovariates)
alphaO_q2.5 = summary[summary$variable=="alphaO",]$q2.5 * range + responseMin - sum(betasO * meanCovariates)
alphaO_q97.5 = summary[summary$variable=="alphaO",]$q97.5 * range + responseMin - sum(betasO * meanCovariates)

alphaD = summary[summary$variable=="alphaD",]$mean * range + responseMin - sum(betasD * meanCovariates)
alphaD_q2.5 = summary[summary$variable=="alphaD",]$q2.5 * range + responseMin - sum(betasD * meanCovariates)
alphaD_q97.5 = summary[summary$variable=="alphaD",]$q97.5 * range + responseMin - sum(betasD * meanCovariates)

sigma = summary[summary$variable=="sigma",]$mean * range
sigma_q2.5 = summary[summary$variable=="sigma",]$q2.5 * range
sigma_q97.5 = summary[summary$variable=="sigma",]$q97.5 * range

output = list(
	betasO = betasO,
	betasO_q2.5 = betasO_q2.5,
	betasO_q97.5 = betasO_q97.5,
	betasD = betasD,
	betasD_q2.5 = betasD_q2.5,
	betasD_q97.5 = betasD_q97.5,
	anchorOnset = anchorOnset,
	anchorOnset_q2.5 = anchorOnset_q2.5,
	anchorOnset_q97.5 = anchorOnset_q97.5,
	anchorDuration = anchorDuration,
	anchorDuration_q2.5 = anchorDuration_q2.5,
	anchorDuration_q97.5 = anchorDuration_q97.5,
	alphaO = alphaO,
	alphaO_q2.5 = alphaO_q2.5,
	alphaO_q97.5 = alphaO_q97.5,
	alphaD = alphaD,
	alphaD_q2.5 = alphaD_q2.5,
	alphaD_q97.5 = alphaD_q97.5,
	sigma = sigma,
	sigma_q2.5 = sigma_q2.5,
	sigma_q97.5 = sigma_q97.5
	)
return(output)
}
