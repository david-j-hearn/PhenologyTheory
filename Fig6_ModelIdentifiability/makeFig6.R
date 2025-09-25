library(ggplot2)
library(cowplot)
source("phenologyVisualization.R")
source("phenologyInference.BB.R")
source("phenologyInference.GP.R")

set.seed(123)

data = read.table("results.I.txt", header=T, sep='\t')

data = data[data$Number_Divergences==0,]
data = data[data$TrueParam_MeanOnset<260,] 		#has a hard time with onsets scrunched up to the end of the year
#data = data[data$TrueParam_MeanOnset>100,]
#data$Collapsed[is.na(data$Collapsed)] = "No"
#data$Collapsed[data$Collapsed==1] = "Yes"
data$Collapsed[data$Est_MLE_MeanDuration<15] = "Yes"
data$Collapsed[data$Est_MLE_MeanDuration>=15] = "No"
data$Rerun[is.na(data$Rerun)] = 0
data = data[data$Rerun==0,]

data = data[sample(nrow(data), 2000), ]

errD.MLE = data$TrueParam_MeanDuration_prescaled_BB - data$Est_MLE_MeanDuration
errD.Stan = data$TrueParam_MeanDuration - data$Est_Stan_HMC_MeanDuration
print(t.test(errD.Stan))
print(t.test(errD.MLE))


#pick a random row with a collapsed duration

matching_rows <- which(data$Collapsed == "Yes")
random_index <- sample(matching_rows, 1)

#n = data$AP_SampleSize[random_index]
n = 1000
N = 500
min = 0
max = 365
nBin = 30
incC = 0.1

#___________________________________________

plotBivariateDiscColor = function(x,y,colorFactVar,x_axis = "Mean Duration", y_axis = "Raw Error", color="Collapsed", highlight = 1, legend=T) {
df <- data.frame(
  x = x,
  y = y,
  group = as.factor(colorFactVar)
)

# Plot with automatic color mapping
if(legend) {
p = ggplot(df, aes(x = x, y = y, color = group)) + 
  scale_color_manual(values = c("No" = "black", "Yes" = "red")) +
  geom_point(size = 1) +
  geom_point(data = df[highlight, ], aes(x, y), color = "yellow", size = 4) +
  labs(x = x_axis, y = y_axis, color = color)+
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 10))  # Top, Right, Bottom, Left
  #theme_minimal()
}
else {
p = ggplot(df, aes(x = x, y = y, color = group)) + 
  scale_color_manual(values = c("No" = "black", "Yes" = "red"), guide="none") +
  geom_point(size = 1) +
  geom_point(data = df[highlight, ], aes(x, y), color = "yellow", size = 4) +
  labs(x = x_axis, y = y_axis, color = color)+
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 10))  # Top, Right, Bottom, Left
  #theme_minimal()
}

return(p)
}



#___________________________________________

#p1 = plotBivariateDiscColor(data$TrueParam_MeanDuration_prescaled_BB, errD.MLE, as.factor( data$Collapsed ), highlight = random_index)
p1 = plotBivariateDiscColor(data$TrueParam_MeanDuration_prescaled_BB, errD.MLE, y_axis = "Raw Error on Mean Duration (MLE BB)", as.factor( data$Collapsed ), highlight = random_index, legend=F)
p2 = plotBivariateDiscColor(data$TrueParam_MeanDuration_prescaled_BB, errD.Stan, y_axis = "Raw Error on Mean Duration (Bayes GP)", as.factor( data$Collapsed ), highlight = random_index)

#pdf("Fig6.p1.pdf")
#print(plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, labels = c("A", "B", "C", "D", "E", "F"), label_x = 0, label_y = 0.1))

figure = plot_grid(p1, p2, nrow = 1, ncol = 2, labels = c("A", "B"), label_x = 0, label_y = 0.1)
save_plot("Fig6.p1.pdf", figure, base_width = 10, base_height = 5)

#dev.off()

#get true and estimated parameters
trueMuO = data$TrueParam_MeanOnset[random_index]
trueSDO = data$TrueParam_SDOnset[random_index]
trueMuD = data$TrueParam_MeanDuration_prescaled_BB[random_index]
trueSDD = data$TrueParam_SDDuration[random_index]
trueMuC = data$TrueParam_MeanCessation[random_index]

estMuO.MLE = data$Est_MLE_MeanOnset[random_index]
estSDO.MLE = data$Est_MLE_SDOnset[random_index]
estMuD.MLE = data$Est_MLE_MeanDuration[random_index]
estSDD.MLE = data$Est_MLE_SDDuration[random_index]
estMuC.MLE = NA

estMuO.GP = data$Est_Stan_HMC_MeanOnset[random_index]
estSDO.GP = data$Est_Stan_HMC_Sigma[random_index]
estMuD.GP = data$Est_Stan_HMC_MeanDuration[random_index]
estSDD.GP = NA
estMuC.GP = data$Est_Stan_HMC_MeanCessation[random_index]

Ts = rT.BB(n=n, mu_O=trueMuO, sigma_O = trueSDO, mu_D = trueMuD, sigma_D = trueSDD, min=min, max=max)

pdf("Fig6.p2.pdf")

layout(matrix(c(1, 2), nrow = 1, byrow = TRUE))
par(mar = c(4, 4, 2, 3))

minX = min(Ts) - 20 
if(minX < min) { minX = min }
maxX = max(Ts) + 20
if(maxX > max) { maxX = max }

makeSimulationInferenceResultsPlot(simData=Ts, trueMuO=trueMuO, trueMuD=trueMuD, trueMuC=trueMuC, trueSDO=trueSDO, trueSDD=trueSDD, estMuO=estMuO.MLE, estMuD=estMuD.MLE, estMuC=estMuC.MLE, estSDO=estSDO.MLE, estSDD=estSDD.MLE, min=min, max=max, N=N, xRange=c(minX,maxX), nBin=nBin, incC=incC, BB.m=T, BB.e=T, GP.m=F, GP.e=F, pdfFile = NA, includeExtremes=F, includeHistogram=F)

#trueMuD = data$TrueParam_MeanDuration[random_index]
makeSimulationInferenceResultsPlot(simData=Ts, trueMuO=trueMuO, trueMuD=trueMuD, trueMuC=trueMuC, trueSDO=trueSDO, trueSDD=trueSDD, estMuO=estMuO.GP, estMuD=estMuD.GP, estMuC=estMuC.GP, estSDO=estSDO.GP, estSDD=estSDD.GP, min=min, max=max, N=N, xRange=c(minX,maxX), nBin=nBin, incC=incC, BB.m=T, BB.e=F, GP.m=F, GP.e=T, pdfFile = NA, includeExtremes=F, includeHistogram=F)

dev.off()
