source("phenologyVisualization.R")

set.seed(12345)

data = read.table("results.CFq.txt", header=T, sep='\t')
data = na.omit(data)
#data = data[sample(nrow(data), 2000), ]
#1000 simulation replicates as reported in MS
data = data[sample(nrow(data), 1000), ]
data = data[data$TrueHyper_mean_MeanDuration-data$TrueParam_MeanDuration!=0,]

attach(data)

errorDuration.stan = TrueParam_MeanDuration-Est_Stan_HMC_MeanDuration

in95CI_MeanDuration = (TrueParam_MeanDuration>Est_Stan_HMC_q2.5_MeanDuration & TrueParam_MeanDuration<Est_Stan_HMC_q97.5_MeanDuration)

percentIn95CI = mean(in95CI_MeanDuration)
cat(100*percentIn95CI)

in95CI_MeanDuration[in95CI_MeanDuration] = "Yes"
in95CI_MeanDuration[in95CI_MeanDuration==F] = "No"

priorBiasDuration = TrueParam_MeanDuration - TrueHyper_mean_MeanDuration

p = plotBivariateDiscColor(x=priorBiasDuration, y=errorDuration.stan, colorFactVar = factor(in95CI_MeanDuration), x_axis="Hyperparameter Mean Duration Bias", y_axis="Raw Error Mean Duration", color="In 95%CI", legend=T , yesCol = "black", noCol = "red")

print(p)

print(summary(lm(errorDuration.stan ~ priorBiasDuration)))

