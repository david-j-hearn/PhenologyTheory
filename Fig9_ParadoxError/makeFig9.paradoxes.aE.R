library(cowplot)
source("heatMap.R")

#data = read.table("results.QP.aE.txt", header=T, sep='\t')
data = read.table("results.X.AE.txt", header=T, sep='\t')

#data$Replicate                              data$Est_SR_Intercept_meanObserved
#data$SampleSize                             data$Est_QR_Slope_meanQ10
#data$ScenarioNumber                         data$Est_QR_Intercept_meanQ10
#data$True_Slope_meanOnset                   data$Est_QR_Slope_meanQ50
#data$True_Intercept_meanOnset               data$Est_QR_Intercept_meanQ50
#data$True_Anchor_meanOnset                  data$Est_QR_Slope_meanQ90
#data$True_Slope_meanDuration                data$Est_QR_Intercept_meanQ90
#data$True_Intercept_meanDuration            data$Est_Stan_Slope_meanOnset
#data$True_Anchor_meanDuration               data$Est_Stan_Slope_meanOnset_q2.5
#data$True_Sigma                             data$Est_Stan_Slope_meanOnset_q97.5
#data$Theor_Slope_meanObserved               data$Est_Stan_Intercept_meanOnset
#data$Theor_Intercept_meanObserved           data$Est_Stan_Intercept_meanOnset_q2.5
#data$Theor_Slope_meanCessation              data$Est_Stan_Intercept_meanOnset_q97.5
#data$Theor_Intercept_meanCessation          data$Est_Stan_Anchor_meanOnset
#data$TrueObs_meanOnset                      data$Est_Stan_Anchor_meanOnset_q2.5
#data$TrueObs_meanDuration                   data$Est_Stan_Anchor_meanOnset_q97.5
#data$Theor_meanOnset                        data$Est_Stan_Slope_meanDuration
#data$Theor_meanDuration                     data$Est_Stan_Slope_meanDuration_q2.5
#data$Hyper_Mean_Slope_meanOnset             data$Est_Stan_Slope_meanDuration_q97.5
#data$Hyper_SD_Slope_meanOnset               data$Est_Stan_Intercept_meanDuration
#data$Hyper_Mean_Slope_meanDuration          data$Est_Stan_Intercept_meanDuration_q2.5
#data$Hyper_SD_Slope_meanDuration            data$Est_Stan_Intercept_meanDuration_q97.5
#data$Hyper_Mean_Anchor_meanOnset            data$Est_Stan_Anchor_meanDuration
#data$Hyper_SD_Anchor_meanOnset              data$Est_Stan_Anchor_meanDuration_q2.5
#data$Hyper_Mean_Anchor_meanDuration         data$Est_Stan_Anchor_meanDuration_q97.5
#data$Hyper_SD_Anchor_meanDuration           data$Est_Stan_Sigma
#data$Hyper_Mean_Sigma                       data$Est_Stan_Sigma_q2.5
#data$Hyper_SD_Sigma                         data$Est_Stan_Sigma_q97.5
#data$Est_SR_Slope_meanObserved              data$Number_Divergences

#thresh = 0.05
#data = data[abs(data$True_Slope_meanOnset)>=thresh & abs(data$True_Slope_meanDuration)>=thresh,]

#set or rename values
data$Est_QR_Slope_meanOnset = data$Est_QR_Slope_meanQ10
data$Est_QR_Slope_meanDuration = data$Est_QR_Slope_meanQ90 - data$Est_QR_Slope_meanQ10
data$Est_QR_Slope_meanObserved = data$Est_QR_Slope_meanQ50
data$Est_Stan_Slope_meanObserved = data$Est_Stan_Slope_meanOnset + data$Est_Stan_Slope_meanDuration/2

#case1 = data[data$True_Slope_meanDuration > 0 & data$True_Slope_meanOnset > 0 & data$Theor_Slope_meanObserved > 0,]
#case2 = data[-data$True_Slope_meanDuration < 2*data$True_Slope_meanOnset & data$True_Slope_meanDuration < 0 & data$True_Slope_meanOnset > 0 & data$Theor_Slope_meanObserved > 0,]
#case3 = data[-data$True_Slope_meanDuration > 2*data$True_Slope_meanOnset & data$True_Slope_meanDuration < 0 & data$True_Slope_meanOnset > 0 & data$Theor_Slope_meanObserved < 0,]
#case4 = data[data$True_Slope_meanDuration < 0 & data$True_Slope_meanOnset < 0 & data$Theor_Slope_meanObserved < 0,]
#case5 = data[2*data$True_Slope_meanDuration < -data$True_Slope_meanOnset & data$True_Slope_meanDuration > 0 & data$True_Slope_meanOnset < 0 & data$Theor_Slope_meanObserved < 0,]
#case6 = data[data$True_Slope_meanDuration > -2*data$True_Slope_meanOnset & data$True_Slope_meanDuration > 0 & data$True_Slope_meanOnset < 0 & data$Theor_Slope_meanObserved > 0,]

case1 = data[data$ScenarioNumber==1,]
case2 = data[data$ScenarioNumber==2,]
case3 = data[data$ScenarioNumber==3,]
case4 = data[data$ScenarioNumber==4,]
case5 = data[data$ScenarioNumber==5,]
case6 = data[data$ScenarioNumber==6,]

row_labels = c("1: +++", "2: +-+", "3: +--", "4: ---", "5: -+-", "6: -++")
highlight_rows = c(row_labels[3],row_labels[6])
value_digits = 3
lowerColor = "red"
upperColor = "steelblue"

lowerColor1 = "steelblue"
upperColor1 = "red"

res.stan <- data.frame(
  SlopeType = integer(),
  Scenario = integer(),
  PercentCorrect = numeric()
)

res.stan[1,] = c(1,1,mean( case1$Est_Stan_Slope_meanOnset > 0 & case1$Est_Stan_Slope_meanDuration > 0 & case1$Est_Stan_Slope_meanObserved > 0))
res.stan[2,] = c(2,1,mean( case1$Est_Stan_Slope_meanOnset > 0 ))
res.stan[3,] = c(3,1,mean( case1$Est_Stan_Slope_meanDuration > 0))
res.stan[4,] = c(4,1,mean( case1$Est_Stan_Slope_meanObserved > 0))

res.stan[5,] = c(1,2,mean( case2$Est_Stan_Slope_meanOnset > 0 & case2$Est_Stan_Slope_meanDuration < 0 & case2$Est_Stan_Slope_meanObserved > 0))
res.stan[6,] = c(2,2,mean( case2$Est_Stan_Slope_meanOnset > 0 ))
res.stan[7,] = c(3,2,mean( case2$Est_Stan_Slope_meanDuration < 0))
res.stan[8,] = c(4,2,mean( case2$Est_Stan_Slope_meanObserved > 0))

res.stan[9,] = c(1,3,mean( case3$Est_Stan_Slope_meanOnset > 0 & case3$Est_Stan_Slope_meanDuration < 0 & case3$Est_Stan_Slope_meanObserved < 0))
res.stan[10,] = c(2,3,mean( case3$Est_Stan_Slope_meanOnset > 0 ))
res.stan[11,] = c(3,3,mean( case3$Est_Stan_Slope_meanDuration < 0))
res.stan[12,] = c(4,3,mean( case3$Est_Stan_Slope_meanObserved < 0))

res.stan[13,] = c(1,4,mean( case4$Est_Stan_Slope_meanOnset < 0 & case4$Est_Stan_Slope_meanDuration < 0 & case4$Est_Stan_Slope_meanObserved < 0))
res.stan[14,] = c(2,4,mean( case4$Est_Stan_Slope_meanOnset < 0 ))
res.stan[15,] = c(3,4,mean( case4$Est_Stan_Slope_meanDuration < 0))
res.stan[16,] = c(4,4,mean( case4$Est_Stan_Slope_meanObserved < 0))

res.stan[17,] = c(1,5,mean( case5$Est_Stan_Slope_meanOnset < 0 & case5$Est_Stan_Slope_meanDuration > 0 & case5$Est_Stan_Slope_meanObserved < 0))
res.stan[18,] = c(2,5,mean( case5$Est_Stan_Slope_meanOnset < 0 ))
res.stan[19,] = c(3,5,mean( case5$Est_Stan_Slope_meanDuration > 0))
res.stan[20,] = c(4,5,mean( case5$Est_Stan_Slope_meanObserved < 0))

res.stan[21,] = c(1,6,mean( case6$Est_Stan_Slope_meanOnset < 0 & case6$Est_Stan_Slope_meanDuration > 0 & case6$Est_Stan_Slope_meanObserved > 0))
res.stan[22,] = c(2,6,mean( case6$Est_Stan_Slope_meanOnset < 0 ))
res.stan[23,] = c(3,6,mean( case6$Est_Stan_Slope_meanDuration > 0))
res.stan[24,] = c(4,6,mean( case6$Est_Stan_Slope_meanObserved > 0))

p.stan = plot_heatmap_from_df(df=res.stan,
                                 row_var = "Scenario",
                                 col_var = "SlopeType",
                                 value_var = "PercentCorrect",
                                 row_labels = row_labels,
                                 col_labels = c("S","O","D","T"),
                                 row_axis_label = "Scenario",
                                 col_axis_label = "Slope Type",
                                 low_color = lowerColor,
                                 high_color = upperColor,
                                 midpoint = NULL,
                                 show_values = TRUE,
                                 value_digits = value_digits,
								 highlight_rows = highlight_rows,
				 show_legend = F,
                                 title = NULL) 

res.q <- data.frame(
  SlopeType = integer(),
  Scenario = integer(),
  PercentCorrect = numeric()
)

res.q[1,] = c(1,1,mean( case1$Est_QR_Slope_meanQ10 > 0 & case1$Est_QR_Slope_meanQ90 - case1$Est_QR_Slope_meanQ10 > 0 & case1$Est_QR_Slope_meanQ50 > 0 ))
res.q[2,] = c(2,1,mean(case1$Est_QR_Slope_meanQ10 > 0))
res.q[3,] = c(3,1,mean(case1$Est_QR_Slope_meanQ90 - case1$Est_QR_Slope_meanQ10 > 0 ))
res.q[4,] = c(4,1,mean(case1$Est_QR_Slope_meanQ50 > 0))

res.q[5,] = c(1,2,mean( case2$Est_QR_Slope_meanQ10 > 0 & case2$Est_QR_Slope_meanQ90 - case2$Est_QR_Slope_meanQ10 < 0 & case2$Est_QR_Slope_meanQ50 > 0 ))
res.q[6,] = c(2,2,mean(case2$Est_QR_Slope_meanQ10 > 0))
res.q[7,] = c(3,2,mean(case2$Est_QR_Slope_meanQ90 - case2$Est_QR_Slope_meanQ10 < 0 ))
res.q[8,] = c(4,2,mean(case2$Est_QR_Slope_meanQ50 > 0))

res.q[9,] = c(1,3,mean( case3$Est_QR_Slope_meanQ10 > 0 & case3$Est_QR_Slope_meanQ90 - case3$Est_QR_Slope_meanQ10 < 0 & case3$Est_QR_Slope_meanQ50 < 0 ))
res.q[10,] = c(2,3,mean(case3$Est_QR_Slope_meanQ10 > 0))
res.q[11,] = c(3,3,mean(case3$Est_QR_Slope_meanQ90 - case3$Est_QR_Slope_meanQ10 < 0 ))
res.q[12,] = c(4,3,mean(case3$Est_QR_Slope_meanQ50 < 0))

res.q[13,] = c(1,4,mean( case4$Est_QR_Slope_meanQ10 < 0 & case4$Est_QR_Slope_meanQ90 - case4$Est_QR_Slope_meanQ10 < 0 & case4$Est_QR_Slope_meanQ50 < 0 ))
res.q[14,] = c(2,4,mean(case4$Est_QR_Slope_meanQ10 < 0))
res.q[15,] = c(3,4,mean(case4$Est_QR_Slope_meanQ90 - case4$Est_QR_Slope_meanQ10 < 0 ))
res.q[16,] = c(4,4,mean(case4$Est_QR_Slope_meanQ50 < 0))

res.q[17,] = c(1,5,mean( case5$Est_QR_Slope_meanQ10 < 0 & case5$Est_QR_Slope_meanQ90 - case5$Est_QR_Slope_meanQ10 > 0 & case5$Est_QR_Slope_meanQ50 < 0 ))
res.q[18,] = c(2,5,mean(case5$Est_QR_Slope_meanQ10 < 0))
res.q[19,] = c(3,5,mean(case5$Est_QR_Slope_meanQ90 - case5$Est_QR_Slope_meanQ10 > 0 ))
res.q[20,] = c(4,5,mean(case5$Est_QR_Slope_meanQ50 < 0))

res.q[21,] = c(1,6,mean( case6$Est_QR_Slope_meanQ10 < 0 & case6$Est_QR_Slope_meanQ90 - case6$Est_QR_Slope_meanQ10 > 0 & case6$Est_QR_Slope_meanQ50 > 0 ))
res.q[22,] = c(2,6,mean(case6$Est_QR_Slope_meanQ10 < 0))
res.q[23,] = c(3,6,mean(case6$Est_QR_Slope_meanQ90 - case6$Est_QR_Slope_meanQ10 > 0 ))
res.q[24,] = c(4,6,mean(case6$Est_QR_Slope_meanQ50 > 0))


p.q = plot_heatmap_from_df(df=res.q,
                                 row_var = "Scenario",
                                 col_var = "SlopeType",
                                 value_var = "PercentCorrect",
                                 row_labels = row_labels,
                                 col_labels = c("S","O","D","T"),
                                 row_axis_label = "Scenario",
                                 col_axis_label = "Slope Type",
                                 low_color = lowerColor,
                                 high_color = upperColor,
                                 midpoint = NULL,
                                 show_values = TRUE,
                                 value_digits = value_digits,
								 highlight_rows = highlight_rows,
				 show_legend = F,
                                 title = NULL) 

res.sr <- data.frame(
  SlopeType = integer(),
  Scenario = integer(),
  PercentCorrect = numeric()
)

#res.sr[1,] = c(1,1,0)
res.sr[1,] = c(1,1,mean(case1$Est_SR_Slope_meanObserved > 0))
#res.sr[3,] = c(3,1,0)
res.sr[2,] = c(2,1,mean(case1$Est_SR_Slope_meanObserved > 0))

#res.sr[5,] = c(1,2,0)
res.sr[3,] = c(1,2,mean(case2$Est_SR_Slope_meanObserved > 0))
#res.sr[7,] = c(3,2,0)
res.sr[4,] = c(2,2,mean(case2$Est_SR_Slope_meanObserved > 0))

#res.sr[9,] = c(1,3,0)
res.sr[5,] = c(1,3,mean(case3$Est_SR_Slope_meanObserved > 0))
#res.sr[11,] = c(3,3,0)
res.sr[6,] = c(2,3,mean(case3$Est_SR_Slope_meanObserved < 0))

#res.sr[13,] = c(1,4,0)
res.sr[7,] = c(1,4,mean(case4$Est_SR_Slope_meanObserved < 0))
#res.sr[15,] = c(3,4,0)
res.sr[8,] = c(2,4,mean(case4$Est_SR_Slope_meanObserved < 0))

#res.sr[17,] = c(1,5,0)
res.sr[9,] = c(1,5,mean(case5$Est_SR_Slope_meanObserved < 0))
#res.sr[19,] = c(3,5,0)
res.sr[10,] = c(2,5,mean(case5$Est_SR_Slope_meanObserved < 0))

#res.sr[21,] = c(1,6,0)
res.sr[11,] = c(1,6,mean(case6$Est_SR_Slope_meanObserved < 0))
#res.sr[23,] = c(3,6,0)
res.sr[12,] = c(2,6,mean(case6$Est_SR_Slope_meanObserved > 0))

p.sr = plot_heatmap_from_df(df=res.sr,
                                 row_var = "Scenario",
                                 col_var = "SlopeType",
                                 value_var = "PercentCorrect",
                                 row_labels = row_labels,
                                 #col_labels = c("S","O","D","T"),
                                 col_labels = c("O","T"),
                                 row_axis_label = "Scenario",
                                 col_axis_label = "Slope Type",
                                 low_color = lowerColor,
                                 high_color = upperColor,
                                 midpoint = NULL,
                                 show_values = TRUE,
                                 value_digits = value_digits,
								 show_legend = T,
								 highlight_rows = highlight_rows,
                                 title = NULL) 


figure = plot_grid(p.stan, p.q, p.sr, nrow = 1, ncol = 3, labels = c("A", "B", "C"), label_x = 0, label_y = 0.1)
save_plot("Fig9.sign.pdf", figure, base_width = 15, base_height = 5)
dev.off()

c1.stan.eO = (abs(case1$True_Slope_meanOnset - case1$Est_Stan_Slope_meanOnset))
c1.stan.eD = (abs(case1$True_Slope_meanDuration - case1$Est_Stan_Slope_meanDuration))
c1.stan.eT = (abs(case1$Theor_Slope_meanObserved - case1$Est_Stan_Slope_meanObserved))
c2.stan.eO = (abs(case2$True_Slope_meanOnset - case2$Est_Stan_Slope_meanOnset))
c2.stan.eD = (abs(case2$True_Slope_meanDuration - case2$Est_Stan_Slope_meanDuration))
c2.stan.eT = (abs(case2$Theor_Slope_meanObserved - case2$Est_Stan_Slope_meanObserved))
c3.stan.eO = (abs(case3$True_Slope_meanOnset - case3$Est_Stan_Slope_meanOnset))
c3.stan.eD = (abs(case3$True_Slope_meanDuration - case3$Est_Stan_Slope_meanDuration))
c3.stan.eT = (abs(case3$Theor_Slope_meanObserved - case3$Est_Stan_Slope_meanObserved))
c4.stan.eO = (abs(case4$True_Slope_meanOnset - case4$Est_Stan_Slope_meanOnset))
c4.stan.eD = (abs(case4$True_Slope_meanDuration - case4$Est_Stan_Slope_meanDuration))
c4.stan.eT = (abs(case4$Theor_Slope_meanObserved - case4$Est_Stan_Slope_meanObserved))
c5.stan.eO = (abs(case5$True_Slope_meanOnset - case5$Est_Stan_Slope_meanOnset))
c5.stan.eD = (abs(case5$True_Slope_meanDuration - case5$Est_Stan_Slope_meanDuration))
c5.stan.eT = (abs(case5$Theor_Slope_meanObserved - case5$Est_Stan_Slope_meanObserved))
c6.stan.eO = (abs(case6$True_Slope_meanOnset - case6$Est_Stan_Slope_meanOnset))
c6.stan.eD = (abs(case6$True_Slope_meanDuration - case6$Est_Stan_Slope_meanDuration))
c6.stan.eT = (abs(case6$Theor_Slope_meanObserved - case6$Est_Stan_Slope_meanObserved))

c1.q.eO = (abs(case1$True_Slope_meanOnset - case1$Est_QR_Slope_meanOnset))
c1.q.eD = (abs(case1$True_Slope_meanDuration - case1$Est_QR_Slope_meanDuration))
c1.q.eT = (abs(case1$Theor_Slope_meanObserved - case1$Est_QR_Slope_meanObserved))
c2.q.eO = (abs(case2$True_Slope_meanOnset - case2$Est_QR_Slope_meanOnset))
c2.q.eD = (abs(case2$True_Slope_meanDuration - case2$Est_QR_Slope_meanDuration))
c2.q.eT = (abs(case2$Theor_Slope_meanObserved - case2$Est_QR_Slope_meanObserved))
c3.q.eO = (abs(case3$True_Slope_meanOnset - case3$Est_QR_Slope_meanOnset))
c3.q.eD = (abs(case3$True_Slope_meanDuration - case3$Est_QR_Slope_meanDuration))
c3.q.eT = (abs(case3$Theor_Slope_meanObserved - case3$Est_QR_Slope_meanObserved))
c4.q.eO = (abs(case4$True_Slope_meanOnset - case4$Est_QR_Slope_meanOnset))
c4.q.eD = (abs(case4$True_Slope_meanDuration - case4$Est_QR_Slope_meanDuration))
c4.q.eT = (abs(case4$Theor_Slope_meanObserved - case4$Est_QR_Slope_meanObserved))
c5.q.eO = (abs(case5$True_Slope_meanOnset - case5$Est_QR_Slope_meanOnset))
c5.q.eD = (abs(case5$True_Slope_meanDuration - case5$Est_QR_Slope_meanDuration))
c5.q.eT = (abs(case5$Theor_Slope_meanObserved - case5$Est_QR_Slope_meanObserved))
c6.q.eO = (abs(case6$True_Slope_meanOnset - case6$Est_QR_Slope_meanOnset))
c6.q.eD = (abs(case6$True_Slope_meanDuration - case6$Est_QR_Slope_meanDuration))
c6.q.eT = (abs(case6$Theor_Slope_meanObserved - case6$Est_QR_Slope_meanObserved))

c1.sr.eO = (abs(case1$True_Slope_meanOnset - case1$Est_SR_Slope_meanObserved))
c1.sr.eD = 1
c1.sr.eT = (abs(case1$Theor_Slope_meanObserved - case1$Est_SR_Slope_meanObserved))
c2.sr.eO = (abs(case2$True_Slope_meanOnset - case2$Est_SR_Slope_meanObserved))
c2.sr.eD = 1
c2.sr.eT = (abs(case2$Theor_Slope_meanObserved - case2$Est_SR_Slope_meanObserved))
c3.sr.eO = (abs(case3$True_Slope_meanOnset - case3$Est_SR_Slope_meanObserved))
c3.sr.eD = 1
c3.sr.eT = (abs(case3$Theor_Slope_meanObserved - case3$Est_SR_Slope_meanObserved))
c4.sr.eO = (abs(case4$True_Slope_meanOnset - case4$Est_SR_Slope_meanObserved))
c4.sr.eD = 1
c4.sr.eT = (abs(case4$Theor_Slope_meanObserved - case4$Est_SR_Slope_meanObserved))
c5.sr.eO = (abs(case5$True_Slope_meanOnset - case5$Est_SR_Slope_meanObserved))
c5.sr.eD = 1
c5.sr.eT = (abs(case5$Theor_Slope_meanObserved - case5$Est_SR_Slope_meanObserved))
c6.sr.eO = (abs(case6$True_Slope_meanOnset - case6$Est_SR_Slope_meanObserved))
c6.sr.eD = 1
c6.sr.eT = (abs(case6$Theor_Slope_meanObserved - case6$Est_SR_Slope_meanObserved))

#max.eO = max(mean(c1.q.eO), mean(c2.q.eO), mean(c3.q.eO), mean(c4.q.eO), mean(c5.q.eO), mean(c6.q.eO), mean(c1.stan.eO), mean(c2.stan.eO), mean(c3.stan.eO), mean(c4.stan.eO), mean(c5.stan.eO), mean(c6.stan.eO))
#max.eO.sr = max(mean(c1.sr.eO), mean(c2.sr.eO), mean(c3.sr.eO), mean(c4.sr.eO), mean(c5.sr.eO), mean(c6.sr.eO))
#min.eO = min(mean(c1.q.eO), mean(c2.q.eO), mean(c3.q.eO), mean(c4.q.eO), mean(c5.q.eO), mean(c6.q.eO), mean(c1.stan.eO), mean(c2.stan.eO), mean(c3.stan.eO), mean(c4.stan.eO), mean(c5.stan.eO), mean(c6.stan.eO))
#min.eO.sr = min(mean(c1.sr.eO), mean(c2.sr.eO), mean(c3.sr.eO), mean(c4.sr.eO), mean(c5.sr.eO), mean(c6.sr.eO))
#max.eD = max(mean(c1.q.eD), mean(c2.q.eD), mean(c3.q.eD), mean(c4.q.eD), mean(c5.q.eD), mean(c6.q.eD), mean(c1.stan.eD), mean(c2.stan.eD), mean(c3.stan.eD), mean(c4.stan.eD), mean(c5.stan.eD), mean(c6.stan.eD))
#min.eD = min(mean(c1.q.eD), mean(c2.q.eD), mean(c3.q.eD), mean(c4.q.eD), mean(c5.q.eD), mean(c6.q.eD), mean(c1.stan.eD), mean(c2.stan.eD), mean(c3.stan.eD), mean(c4.stan.eD), mean(c5.stan.eD), mean(c6.stan.eD))
#max.eT = max(mean(c1.q.eT), mean(c2.q.eT), mean(c3.q.eT), mean(c4.q.eT), mean(c5.q.eT), mean(c6.q.eT), mean(c1.stan.eT), mean(c2.stan.eT), mean(c3.stan.eT), mean(c4.stan.eT), mean(c5.stan.eT), mean(c6.stan.eT))
#max.eT.sr = max(mean(c1.sr.eT), mean(c2.sr.eT), mean(c3.sr.eT), mean(c4.sr.eT), mean(c5.sr.eT), mean(c6.sr.eT))
#min.eT = min(mean(c1.q.eT), mean(c2.q.eT), mean(c3.q.eT), mean(c4.q.eT), mean(c5.q.eT), mean(c6.q.eT), mean(c1.stan.eT), mean(c2.stan.eT), mean(c3.stan.eT), mean(c4.stan.eT), mean(c5.stan.eT), mean(c6.stan.eT))
#min.eT.sr = min(mean(c1.sr.eT), mean(c2.sr.eT), mean(c3.sr.eT), mean(c4.sr.eT), mean(c5.sr.eT), mean(c6.sr.eT))

c1.stan.eO = (mean(c1.stan.eO))
c1.stan.eD = (mean(c1.stan.eD))
c1.stan.eT = (mean(c1.stan.eT))
c2.stan.eO = (mean(c2.stan.eO))
c2.stan.eD = (mean(c2.stan.eD))
c2.stan.eT = (mean(c2.stan.eT))
c3.stan.eO = (mean(c3.stan.eO))
c3.stan.eD = (mean(c3.stan.eD))
c3.stan.eT = (mean(c3.stan.eT))
c4.stan.eO = (mean(c4.stan.eO))
c4.stan.eD = (mean(c4.stan.eD))
c4.stan.eT = (mean(c4.stan.eT))
c5.stan.eO = (mean(c5.stan.eO))
c5.stan.eD = (mean(c5.stan.eD))
c5.stan.eT = (mean(c5.stan.eT))
c6.stan.eO = (mean(c6.stan.eO))
c6.stan.eD = (mean(c6.stan.eD))
c6.stan.eT = (mean(c6.stan.eT))

#print("Stan mean errors")
#print(c(c1.stan.eO, c1.stan.eD, c1.stan.eT))

c1.q.eO = (mean(c1.q.eO))
c1.q.eD = (mean(c1.q.eD))
c1.q.eT = (mean(c1.q.eT))
c2.q.eO = (mean(c2.q.eO))
c2.q.eD = (mean(c2.q.eD))
c2.q.eT = (mean(c2.q.eT))
c3.q.eO = (mean(c3.q.eO))
c3.q.eD = (mean(c3.q.eD))
c3.q.eT = (mean(c3.q.eT))
c4.q.eO = (mean(c4.q.eO))
c4.q.eD = (mean(c4.q.eD))
c4.q.eT = (mean(c4.q.eT))
c5.q.eO = (mean(c5.q.eO))
c5.q.eD = (mean(c5.q.eD))
c5.q.eT = (mean(c5.q.eT))
c6.q.eO = (mean(c6.q.eO))
c6.q.eD = (mean(c6.q.eD))
c6.q.eT = (mean(c6.q.eT))

#print("QR mean errors")
#print(c(c1.q.eO, c1.q.eD, c1.q.eT))

c1.sr.eO = (mean(c1.sr.eO))
c1.sr.eT = (mean(c1.sr.eT))
c2.sr.eO = (mean(c2.sr.eO))
c2.sr.eT = (mean(c2.sr.eT))
c3.sr.eO = (mean(c3.sr.eO))
c3.sr.eT = (mean(c3.sr.eT))
c4.sr.eO = (mean(c4.sr.eO))
c4.sr.eT = (mean(c4.sr.eT))
c5.sr.eO = (mean(c5.sr.eO))
c5.sr.eT = (mean(c5.sr.eT))
c6.sr.eO = (mean(c6.sr.eO))
c6.sr.eT = (mean(c6.sr.eT))

maxE = max(c1.stan.eO, c1.stan.eD, c1.stan.eT, c2.stan.eO, c2.stan.eD, c2.stan.eT, c3.stan.eO, c3.stan.eD, c3.stan.eT, c4.stan.eO, c4.stan.eD, c4.stan.eT, c5.stan.eO, c5.stan.eD, c5.stan.eT, c6.stan.eO, c6.stan.eD, c6.stan.eT, c1.q.eO, c1.q.eD, c1.q.eT, c2.q.eO, c2.q.eD, c2.q.eT, c3.q.eO, c3.q.eD, c3.q.eT, c4.q.eO, c4.q.eD, c4.q.eT, c5.q.eO, c5.q.eD, c5.q.eT, c6.q.eO, c6.q.eD, c6.q.eT, c1.sr.eO, c1.sr.eT, c2.sr.eO, c2.sr.eT, c3.sr.eO, c3.sr.eT, c4.sr.eO, c4.sr.eT, c5.sr.eO, c5.sr.eT, c6.sr.eO, c6.sr.eT)

#c1.stan.eO = (mean(c1.stan.eO) - min.eO) / (max.eO - min.eO)
#c1.stan.eD = (mean(c1.stan.eD) - min.eD) / (max.eD - min.eD)
#c1.stan.eT = (mean(c1.stan.eT) - min.eT) / (max.eT - min.eT)
#c2.stan.eO = (mean(c2.stan.eO) - min.eO) / (max.eO - min.eO)
#c2.stan.eD = (mean(c2.stan.eD) - min.eD) / (max.eD - min.eD)
#c2.stan.eT = (mean(c2.stan.eT) - min.eT) / (max.eT - min.eT)
#c3.stan.eO = (mean(c3.stan.eO) - min.eO) / (max.eO - min.eO)
#c3.stan.eD = (mean(c3.stan.eD) - min.eD) / (max.eD - min.eD)
#c3.stan.eT = (mean(c3.stan.eT) - min.eT) / (max.eT - min.eT)
#c4.stan.eO = (mean(c4.stan.eO) - min.eO) / (max.eO - min.eO)
#c4.stan.eD = (mean(c4.stan.eD) - min.eD) / (max.eD - min.eD)
#c4.stan.eT = (mean(c4.stan.eT) - min.eT) / (max.eT - min.eT)
#c5.stan.eO = (mean(c5.stan.eO) - min.eO) / (max.eO - min.eO)
#c5.stan.eD = (mean(c5.stan.eD) - min.eD) / (max.eD - min.eD)
#c5.stan.eT = (mean(c5.stan.eT) - min.eT) / (max.eT - min.eT)
#c6.stan.eO = (mean(c6.stan.eO) - min.eO) / (max.eO - min.eO)
#c6.stan.eD = (mean(c6.stan.eD) - min.eD) / (max.eD - min.eD)
#c6.stan.eT = (mean(c6.stan.eT) - min.eT) / (max.eT - min.eT)
#
##print("Stan mean errors")
##print(c(c1.stan.eO, c1.stan.eD, c1.stan.eT))
#
#c1.q.eO = (mean(c1.q.eO) - min.eO) / (max.eO - min.eO)
#c1.q.eD = (mean(c1.q.eD) - min.eD) / (max.eD - min.eD)
#c1.q.eT = (mean(c1.q.eT) - min.eT) / (max.eT - min.eT)
#c2.q.eO = (mean(c2.q.eO) - min.eO) / (max.eO - min.eO)
#c2.q.eD = (mean(c2.q.eD) - min.eD) / (max.eD - min.eD)
#c2.q.eT = (mean(c2.q.eT) - min.eT) / (max.eT - min.eT)
#c3.q.eO = (mean(c3.q.eO) - min.eO) / (max.eO - min.eO)
#c3.q.eD = (mean(c3.q.eD) - min.eD) / (max.eD - min.eD)
#c3.q.eT = (mean(c3.q.eT) - min.eT) / (max.eT - min.eT)
#c4.q.eO = (mean(c4.q.eO) - min.eO) / (max.eO - min.eO)
#c4.q.eD = (mean(c4.q.eD) - min.eD) / (max.eD - min.eD)
#c4.q.eT = (mean(c4.q.eT) - min.eT) / (max.eT - min.eT)
#c5.q.eO = (mean(c5.q.eO) - min.eO) / (max.eO - min.eO)
#c5.q.eD = (mean(c5.q.eD) - min.eD) / (max.eD - min.eD)
#c5.q.eT = (mean(c5.q.eT) - min.eT) / (max.eT - min.eT)
#c6.q.eO = (mean(c6.q.eO) - min.eO) / (max.eO - min.eO)
#c6.q.eD = (mean(c6.q.eD) - min.eD) / (max.eD - min.eD)
#c6.q.eT = (mean(c6.q.eT) - min.eT) / (max.eT - min.eT)
#
##print("QR mean errors")
##print(c(c1.q.eO, c1.q.eD, c1.q.eT))
#
#c1.sr.eO = (mean(c1.sr.eO) - min.eO) / (max.eO.sr - min.eO.sr)
#c1.sr.eT = (mean(c1.sr.eT) - min.eT) / (max.eT.sr - min.eT.sr)
#c2.sr.eO = (mean(c2.sr.eO) - min.eO) / (max.eO.sr - min.eO.sr)
#c2.sr.eT = (mean(c2.sr.eT) - min.eT) / (max.eT.sr - min.eT.sr)
#c3.sr.eO = (mean(c3.sr.eO) - min.eO) / (max.eO.sr - min.eO.sr)
#c3.sr.eT = (mean(c3.sr.eT) - min.eT) / (max.eT.sr - min.eT.sr)
#c4.sr.eO = (mean(c4.sr.eO) - min.eO) / (max.eO.sr - min.eO.sr)
#c4.sr.eT = (mean(c4.sr.eT) - min.eT) / (max.eT.sr - min.eT.sr)
#c5.sr.eO = (mean(c5.sr.eO) - min.eO) / (max.eO.sr - min.eO.sr)
#c5.sr.eT = (mean(c5.sr.eT) - min.eT) / (max.eT.sr - min.eT.sr)
#c6.sr.eO = (mean(c6.sr.eO) - min.eO) / (max.eO.sr - min.eO.sr)
#c6.sr.eT = (mean(c6.sr.eT) - min.eT) / (max.eT.sr - min.eT.sr)

#print("SR mean errors")
#print(c(c1.sr.eO, c1.sr.eD, c1.sr.eT))

res.stan <- data.frame(
  SlopeType = integer(),
  Scenario = integer(),
  PercentCorrect = numeric()
)

#res.stan[1,] = c(1,1,mean( case1$Est_Stan_Slope_meanOnset > 0 & case1$Est_Stan_Slope_meanDuration > 0 & case1$Est_Stan_Slope_meanObserved > 0))
res.stan[1,] = c(1,1,c1.stan.eO)
res.stan[2,] = c(2,1,c1.stan.eD)
res.stan[3,] = c(3,1,c1.stan.eT)

#res.stan[5,] = c(1,2,mean( case2$Est_Stan_Slope_meanOnset > 0 & case2$Est_Stan_Slope_meanDuration < 0 & case2$Est_Stan_Slope_meanObserved > 0))
res.stan[4,] = c(1,2,c2.stan.eO)
res.stan[5,] = c(2,2,c2.stan.eD)
res.stan[6,] = c(3,2,c2.stan.eT)

#res.stan[9,] = c(1,3,mean( case3$Est_Stan_Slope_meanOnset > 0 & case3$Est_Stan_Slope_meanDuration < 0 & case3$Est_Stan_Slope_meanObserved < 0))
res.stan[7,] = c(1,3,c3.stan.eO)
res.stan[8,] = c(2,3,c3.stan.eD)
res.stan[9,] = c(3,3,c3.stan.eT)

#res.stan[13,] = c(1,4,mean( case4$Est_Stan_Slope_meanOnset < 0 & case4$Est_Stan_Slope_meanDuration < 0 & case4$Est_Stan_Slope_meanObserved < 0))
res.stan[10,] = c(1,4,c4.stan.eO)
res.stan[11,] = c(2,4,c4.stan.eD)
res.stan[12,] = c(3,4,c4.stan.eT)

#res.stan[17,] = c(1,5,mean( case5$Est_Stan_Slope_meanOnset < 0 & case5$Est_Stan_Slope_meanDuration > 0 & case5$Est_Stan_Slope_meanObserved < 0))
res.stan[13,] = c(1,5,c5.stan.eO)
res.stan[14,] = c(2,5,c5.stan.eD)
res.stan[15,] = c(3,5,c5.stan.eT)

#res.stan[21,] = c(1,6,mean( case6$Est_Stan_Slope_meanOnset < 0 & case6$Est_Stan_Slope_meanDuration > 0 & case6$Est_Stan_Slope_meanObserved > 0))
res.stan[16,] = c(1,6,c6.stan.eO)
res.stan[17,] = c(2,6,c6.stan.eD)
res.stan[18,] = c(3,6,c6.stan.eT)

print("res.stan")
p.stan = plot_heatmap_from_df(df=res.stan,
                                 row_var = "Scenario",
                                 col_var = "SlopeType",
                                 value_var = "PercentCorrect",
                                 row_labels = row_labels,
                                 #col_labels = c("S","O","D","T"),
                                 col_labels = c("O","D","T"),
                                 row_axis_label = "Scenario",
                                 col_axis_label = "Slope Type",
                                 low_color = lowerColor1,
                                 high_color = upperColor1,
                                 midpoint = NULL,
                                 show_values = TRUE,
                                 value_digits = value_digits,
								 limits = c(0, maxE),
								 highlight_rows = highlight_rows,
				 show_legend = F,
                                 title = NULL) 


res.q <- data.frame(
  SlopeType = integer(),
  Scenario = integer(),
  PercentCorrect = numeric()
)
#res.q[1,] = c(1,1,mean( case1$Est_QR_Slope_meanOnset > 0 & case1$Est_QR_Slope_meanDuration > 0 & case1$Est_QR_Slope_meanObserved > 0))
res.q[1,] = c(1,1,c1.q.eO)
res.q[2,] = c(2,1,c1.q.eD)
res.q[3,] = c(3,1,c1.q.eT)

#res.q[5,] = c(1,2,mean( case2$Est_QR_Slope_meanOnset > 0 & case2$Est_QR_Slope_meanDuration < 0 & case2$Est_QR_Slope_meanObserved > 0))
res.q[4,] = c(1,2,c2.q.eO)
res.q[5,] = c(2,2,c2.q.eD)
res.q[6,] = c(3,2,c2.q.eT)

#res.q[9,] = c(1,3,mean( case3$Est_QR_Slope_meanOnset > 0 & case3$Est_QR_Slope_meanDuration < 0 & case3$Est_QR_Slope_meanObserved < 0))
res.q[7,] = c(1,3,c3.q.eO)
res.q[8,] = c(2,3,c3.q.eD)
res.q[9,] = c(3,3,c3.q.eT)

#res.q[13,] = c(1,4,mean( case4$Est_QR_Slope_meanOnset < 0 & case4$Est_QR_Slope_meanDuration < 0 & case4$Est_QR_Slope_meanObserved < 0))
res.q[10,] = c(1,4,c4.q.eO)
res.q[11,] = c(2,4,c4.q.eD)
res.q[12,] = c(3,4,c4.q.eT)

#res.q[17,] = c(1,5,mean( case5$Est_QR_Slope_meanOnset < 0 & case5$Est_QR_Slope_meanDuration > 0 & case5$Est_QR_Slope_meanObserved < 0))
res.q[13,] = c(1,5,c5.q.eO)
res.q[14,] = c(2,5,c5.q.eD)
res.q[15,] = c(3,5,c5.q.eT)

#res.q[21,] = c(1,6,mean( case6$Est_QR_Slope_meanOnset < 0 & case6$Est_QR_Slope_meanDuration > 0 & case6$Est_QR_Slope_meanObserved > 0))
res.q[16,] = c(1,6,c6.q.eO)
res.q[17,] = c(2,6,c6.q.eD)
res.q[18,] = c(3,6,c6.q.eT)

print("res QR")
p.q = plot_heatmap_from_df(df=res.q,
                                 row_var = "Scenario",
                                 col_var = "SlopeType",
                                 value_var = "PercentCorrect",
                                 row_labels = row_labels,
                                 #col_labels = c("S","O","D","T"),
                                 col_labels = c("O","D","T"),
                                 row_axis_label = "Scenario",
                                 col_axis_label = "Slope Type",
                                 low_color = lowerColor1,
                                 high_color = upperColor1,
                                 midpoint = NULL,
                                 show_values = TRUE,
                                 value_digits = value_digits,
								 limits = c(0, maxE),
								 highlight_rows = highlight_rows,
				 show_legend = F,
                                 title = NULL) 


res.sr <- data.frame(
  SlopeType = integer(),
  Scenario = integer(),
  PercentCorrect = numeric()
)
#res.sr[1,] = c(1,1,mean( case1$Est_QR_Slope_meanOnset > 0 & case1$Est_QR_Slope_meanDuration > 0 & case1$Est_QR_Slope_meanObserved > 0))
res.sr[1,] = c(1,1,c1.sr.eO)
#res.sr[3,] = c(3,1,c1.sr.eD)
res.sr[2,] = c(2,1,c1.sr.eT)

#res.sr[5,] = c(1,2,mean( case2$Est_QR_Slope_meanOnset > 0 & case2$Est_QR_Slope_meanDuration < 0 & case2$Est_QR_Slope_meanObserved > 0))
res.sr[3,] = c(1,2,c2.sr.eO)
#res.sr[7,] = c(3,2,c2.sr.eD)
res.sr[4,] = c(2,2,c2.sr.eT)

#res.sr[9,] = c(1,3,mean( case3$Est_QR_Slope_meanOnset > 0 & case3$Est_QR_Slope_meanDuration < 0 & case3$Est_QR_Slope_meanObserved < 0))
res.sr[5,] = c(1,3,c3.sr.eO)
#res.sr[11,] = c(3,3,c3.sr.eD)
res.sr[6,] = c(2,3,c3.sr.eT)

#res.sr[13,] = c(1,4,mean( case4$Est_QR_Slope_meanOnset < 0 & case4$Est_QR_Slope_meanDuration < 0 & case4$Est_QR_Slope_meanObserved < 0))
res.sr[7,] = c(1,4,c4.sr.eO)
#res.sr[15,] = c(3,4,c4.sr.eD)
res.sr[8,] = c(2,4,c4.sr.eT)

#res.sr[17,] = c(1,5,mean( case5$Est_QR_Slope_meanOnset < 0 & case5$Est_QR_Slope_meanDuration > 0 & case5$Est_QR_Slope_meanObserved < 0))
res.sr[9,] = c(1,5,c5.sr.eO)
#res.sr[19,] = c(3,5,c5.sr.eD)
res.sr[10,] = c(2,5,c5.sr.eT)

#res.sr[21,] = c(1,6,mean( case6$Est_QR_Slope_meanOnset < 0 & case6$Est_QR_Slope_meanDuration > 0 & case6$Est_QR_Slope_meanObserved > 0))
res.sr[11,] = c(1,6,c6.sr.eO)
#res.sr[23,] = c(3,6,c6.sr.eD)
res.sr[12,] = c(2,6,c6.sr.eT)

print("res SR")
p.sr = plot_heatmap_from_df(df=res.sr,
                                 row_var = "Scenario",
                                 col_var = "SlopeType",
                                 value_var = "PercentCorrect",
                                 row_labels = row_labels,
                                 #col_labels = c("S","O","D","T"),
                                 col_labels = c("O","T"),
                                 row_axis_label = "Scenario",
                                 col_axis_label = "Slope Type",
                                 low_color = lowerColor1,
                                 high_color = upperColor1,
                                 midpoint = NULL,
                                 show_values = TRUE,
                                 value_digits = value_digits,
								 limits = c(0, maxE),
								 highlight_rows = highlight_rows,
				 show_legend = F,
                                 title = NULL) 

figure = plot_grid(p.stan, p.q, p.sr, nrow = 1, ncol = 3, labels = c("A", "B", "C"), label_x = 0, label_y = 0.1)
save_plot("Fig9.error.pdf", figure, base_width = 15, base_height = 5)
dev.off()
