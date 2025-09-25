
#for each species:

#read the data file
#prepare the data for runStan
#run runStan
#extract the samples
#	prepare the summary information for each parameter: species mean, q2.5, q97.5, rhat, ESS
#	make the posterior predictive plot for the DOY vs. year data
#		shows the regression line for the onset, observed, cessation
#		shows the shift in distribution for onset, duration, observed at an early and a late timepoint at the mean posterior parameter values

#runEmpiricalAnalysis = function(species, responseVariableName="DOY", onsetCovariateNames=c("Year", "Latitude", "Elevation"), durationCovariateNames=c("Year", "Latitude", "Elevation")) {
#runEmpiricalAnalysis = function(species, responseVariableName="DOY", onsetCovariateNames=c("Year", "Latitude", "Elevation"), durationCovariateNames=c("Year", "Latitude", "Elevation")) {
#runEmpiricalAnalysis = function(species, responseVariableName="DOY", onsetCovariateNames=c("FirstQuarterMonthlyAverageTemp", "Latitude", "Elevation", "Year"), durationCovariateNames=c("FirstQuarterMonthlyAverageTemp", "Latitude", "Elevation", "Year")) {

#AnnualMonthlyAverageTemp,    SpringMonthlyAverageTemp,    FirstQuarterMonthlyAverageTemp
runEmpiricalAnalysis = function(species, responseVariableName="DOY", onsetCovariateNames=c("SpringMonthlyAverageTemp", "Latitude", "Elevation", "Year","AnnualMonthlyAverageTemp", "FirstQuarterMonthlyAverageTemp"), durationCovariateNames=c("SpringMonthlyAverageTemp", "Latitude", "Elevation", "Year","AnnualMonthlyAverageTemp","FirstQuarterMonthlyAverageTemp"), resultsDirectory="Results", removeOutliers=FALSE, prepareSummary=TRUE, runStandardLinearModel=TRUE, maximizeSampleSize=FALSE, makePlots=FALSE) {


#runEmpiricalAnalysis = function(species, responseVariableName="DOY", onsetCovariateNames=c("FirstQuarterMonthlyAverageTemp"), durationCovariateNames=c("FirstQuarterMonthlyAverageTemp")) {
	source("phenologyInference.R")
	source("phenologyVisualization.R")

	minResponse=0
	maxResponse=365

		N = 500

	name = species

	if (!dir.exists(resultsDirectory)) {
  dir.create(resultsDirectory, recursive = TRUE, showWarnings = FALSE)
	}





	#species = c("Anemone_quinquefolia", "Camassia_scilloides", "Cardamine_concatenata", "Claytonia_virginica", "Collinsia_verna", "Dicentra_cucullaria", "Enemion_biternatum", "Erythronium_americanum", "Mertensia_virginica", "Podophyllum_peltatum", "Primula_meadia", "Sanguinaria_canadensis", "Thalictrum_thalictroides")

	cnt = 0
	#for(i in 1:length(species)) {
	#for(i in c(2,5)) {
		cnt = cnt+1
		#name = species[i]

		#get the data
		#dataFile = paste0("Data_Flowering/NoOutliers/", name, ".noOutliers.txt")
		dataFile = paste0("climateData/Extract/", name, ".Full.txt")
		originalDataFile = paste0("Data/", name, ".csv")
		raw = read.csv(originalDataFile)
		origN = nrow(raw)
		cat(paste("Processing ", dataFile, "\n"));

		cat("Preparing data from user-supplied file.\n")
		data = preparePhenologyData(dataFile=dataFile,
									responseVariableName=responseVariableName,
									onsetCovariateNames=onsetCovariateNames,
									durationCovariateNames=durationCovariateNames,
									removeDuplicateRows=TRUE,
									removeOutliers=removeOutliers,
									removeIncomplete=TRUE,
									dataSummaryDirectory=resultsDirectory,
									taxonName=name,
									origN = origN
		)
		totalSamples = nrow(data$onsetCovariateData)


		#make the "Year" variable more precise by including the fraction into the year
		#data$onsetCovariateData$Year = data$onsetCovariateData$Year + data$responseData/365
		#data$durationCovariateData$Year = data$durationCovariateData$Year + data$responseData/365

		#print(data$responseData)
		#print(data$onsetCovariateData)
		#print(data$durationCovariateData)

		#if(!is.data.frame(data$durationCovariateData)) {
			#stop("Duration covariate data not a data frame.")
		#}

	if(runStandardLinearModel) {
		fit = runStandardLinearModel(data$responseData, data$onsetCovariateData, data$durationCovariateData)
	}

		#ploting response and first covariate
		plot(data$onsetCovariateData[[1]], data$responseData)

		#run Stan
		cat("Running Stan.\n")
		stanResult = runStanPhenology(
									  type="full",
									  responseData=data$responseData,
									  onsetCovariateData=data$onsetCovariateData,
									  durationCovariateData=data$durationCovariateData,
									  minResponse=minResponse,
									  maxResponse=maxResponse,
									  partitionDataForPriors=TRUE,		#partition the data and use the first partition to estimate hyperparameter values
									  maxDiv=1000,
									  processExtremes=TRUE,
									  N=N
									  #setStringent=TRUE,
									  #hyperparams_noCovariates=NA,
									  #onsetHyperBeta=NA,
									  #onsetHyperAnchor=NA,
									  #durationHyperBeta=NA,
									  #durationHyperAnchor=NA,
									  #sigmaHyper=NA,
									  #threshApprox=NA,
									  #runMap=FALSE,
									  #maximizeSampleSize=TRUE,
		)

		if(stanResult$error) {
			#print(stanResult$errorM)
			stop(paste("The stan run had problems: ", stanResult$errorM))
		}

		print("Summarizing stan result")
		summary = summarizePhenologyResults(stanRunResult=stanResult, taxonName=name, standardLinearModel = fit, N=N)
		sumDiag =  summaryStanDiagnostics(stanRunResult=stanResult, NTotal=totalSamples, taxonName=name)

		#if(cnt == 1) {
			#summaryMaster = summary
			#sumDiagMaster = sumDiag
		#}
		#else {
			#summaryMaster = rbind(summaryMaster, summary)
			#sumDiagMaster = rbind(sumDiagMaster, sumDiag)
		#}
	#}

	write.table(summary, paste0(resultsDirectory, "/StanResultsSummary.", name, ".txt"), row.names=FALSE, quote=FALSE, sep="\t")
	write.table(sumDiag, paste0(resultsDirectory, "/StanDiagnosticsSummary.", name, ".txt"), row.names=FALSE, quote=FALSE, sep="\t")

	if(makePlots) {
	cat("Creating posterior predictive plot.\n")
	targetCovariateNames = c("SpringMonthlyAverageTemp","Year","Latitude","Elevation","AnnualMonthlyAverageTemp")
	for(targetCovariateName in targetCovariateNames) {
		print(targetCovariateName)
	p = makePosteriorPredictivePlot(stanResult=stanResult, 
									responseData=data$responseData, 
									responseVariableName="DOY", 
									#targetCovariateName="Year", 
									targetCovariateName=targetCovariateName, 
									onsetCovariateData=data$onsetCovariateData, 
									durationCovariateData=data$durationCovariateData, 
									n_samples=1000,  #sample of covariate data using Gaussian copula
									n_draws=1500, #sample of HMC draws,
									n_hist=10000, #sample of values from phenology distributions for histograms or to get MC estimates of means
									n_bin=100, #number of bins in histograms
									resolution=160, #resolution of the target covariate in the posterior predictive graph
									slice=0.1, #when time slices (also at 1-slice) for posterior predictive graph histograms should be made
									N=500, #population size used to calculate extremes
									minResponse=minResponse, #smallest possible time for the phenophase
									maxResponse=maxResponse,	#largest possible time for the phenophase
									smooth="GAM"
									#smooth="LOESS"
								)

	ggsave(paste0(resultsDirectory,"/posteriorPredictivePlot.", name, ".",  targetCovariateName, ".pdf"), p, width = 8, height = 6)
	}
	}

	else {
		p = "No posterior predictive plot requested."
	}

	out = list(
			   taxon = name,
			   summaryResults = summary,
			   summaryDiagnostics = sumDiag,
			   posteriorPredictivePlot = p,
			   data = data,
			   stanResult = stanResult
			   )
	return(out)
}

