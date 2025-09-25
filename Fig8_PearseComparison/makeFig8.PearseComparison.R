cat("Reading data\n")

data = read.table("results.txt", header=T, sep='\t')

cat("Filtering and summarizing data\n")
split_tables <- split(data, factor(data$AP_PopulationSize))
p100 = split_tables$"100"
p10000 = split_tables$"10000"
p1000000 = split_tables$"1000000"

p100 = p100[p100$AP_SampleSize<=90,]
p10000 = p10000[p10000$AP_SampleSize<=90,]
p1000000 = p1000000[p1000000$AP_SampleSize<=90,]
x = seq(10,90, by=2)

#data$AP_Replicate                                  data$TrueHyper_sd_MeanDuration                     data$EstTheor_Stan_HMC_q2.5_FirstOnset
#data$AP_PopulationSize                             data$TrueHyper_mean_Sigma                          data$EstTheor_Stan_HMC_q97.5_FirstOnset
#data$AP_SampleSize                                 data$TrueHyper_sd_Sigma                            data$Est_Pearse_FullSample_FirstOnset
#data$Number_Divergences                            data$Est_Stan_HMC_MeanOnset                        data$Est_Pearse_FullSample_q2.5_FirstOnset
#data$Number_Runs_Rejected                          data$Est_Stan_HMC_MeanDuration                     data$Est_Pearse_FullSample_q97.5_FirstOnset
#data$TrueParam_MeanOnset                           data$Est_Stan_HMC_MeanCessation                    data$Est_Pearse_SubSample_FirstOnset
#data$TrueParam_MeanDuration                        data$Est_Stan_HMC_Sigma                            data$Est_Pearse_SubSample_q2.5_FirstOnset
#data$TrueParam_MeanCessation                       data$Est_Stan_HMC_q2.5_MeanOnset                   data$Est_Pearse_SubSample_q97.5_FirstOnset
#data$TrueParam_Sigma                               data$Est_Stan_HMC_q2.5_MeanDuration                data$Est_Pearse_FullSample_FirstOnset_onset
#data$TrueObserved_FirstOnset                       data$Est_Stan_HMC_q2.5_MeanCessation               data$Est_Pearse_FullSample_q2.5_FirstOnset_onset
#data$TheorParam_ExpectedFirstOnset_fromTrueModel   data$Est_Stan_HMC_q2.5_Sigma                       data$Est_Pearse_FullSample_q97.5_FirstOnset_onset
#data$TheorParam_q2.5FirstOnset_fromTrueModel       data$Est_Stan_HMC_q97.5_MeanOnset                  data$Est_Pearse_SubSample_FirstOnset_onset
#data$TheorParam_q97.5FirstOnset_fromTrueModel      data$Est_Stan_HMC_q97.5_MeanDuration               data$Est_Pearse_SubSample_q2.5_FirstOnset_onset
#data$TrueHyper_mean_MeanOnset                      data$Est_Stan_HMC_q97.5_MeanCessation              data$Est_Pearse_SubSample_q97.5_FirstOnset_onset
#data$TrueHyper_sd_MeanOnset                        data$Est_Stan_HMC_q97.5_Sigma                      
#data$TrueHyper_mean_MeanDuration                   data$EstTheor_Stan_HMC_FirstOnset                  


print("MEANS")
Obs.100.m = tapply(p100$TrueObserved_FirstOnset, p100$AP_SampleSize, mean, na.rm = TRUE)
print("OBS")
print(Obs.100.m)
Theor.100.m = tapply(p100$TheorParam_ExpectedFirstOnset_fromTrueModel, p100$AP_SampleSize, mean, na.rm=T)
print("Theor")
print(Theor.100.m)
print("Stan")
Stan.100.m = tapply(p100$EstTheor_Stan_HMC_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print(Stan.100.m)
PFull.100.m = tapply(p100$Est_Pearse_FullSample_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print("PFull")
print(PFull.100.m)
PSub.100.m = tapply(p100$Est_Pearse_SubSample_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print("PSub")
print(PSub.100.m)
PFull_o.100.m = tapply(p100$Est_Pearse_FullSample_FirstOnset_onset, p100$AP_SampleSize, mean, na.rm=T)
print("PFull_o")
print(PFull_o.100.m)
PSub_o.100.m = tapply(p100$Est_Pearse_SubSample_FirstOnset_onset, p100$AP_SampleSize, mean, na.rm=T)
print("PSub_o")
print(PSub_o.100.m)

#print("SDs")
#Obs.100.sd = tapply(p100$TrueObserved_FirstOnset, p100$AP_SampleSize, sd, na.rm = TRUE)
#print("OBS")
#print(Obs.100.sd)
#Theor.100.sd = tapply(p100$TheorParam_ExpectedFirstOnset_fromTrueModel, p100$AP_SampleSize, sd, na.rm=T)
#print("Theor")
#print(Theor.100.sd)
#Stan.100.sd = tapply(p100$EstTheor_Stan_HMC_FirstOnset, p100$AP_SampleSize, sd, na.rm=T)
#print("Stan")
#print(Stan.100.sd)
#PFull.100.sd = tapply(p100$Est_Pearse_FullSample_FirstOnset, p100$AP_SampleSize, sd, na.rm=T)
#print("PFull")
#print(PFull.100.sd)
#PSub.100.sd = tapply(p100$Est_Pearse_SubSample_FirstOnset, p100$AP_SampleSize, sd, na.rm=T)
#print("PSub")
#print(PSub.100.sd)

print("q2.5")
Stan.100.Bq025 = tapply(p100$EstTheor_Stan_HMC_q2.5_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print("Stan")
print(Stan.100.Bq025)
PFull.100.Bq025 = tapply(p100$Est_Pearse_FullSample_q2.5_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print("PFull")
print(PFull.100.Bq025)
PSub.100.Bq025 = tapply(p100$Est_Pearse_SubSample_q2.5_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print("PSub")
print(PSub.100.Bq025)
PFull_o.100.Bq025 = tapply(p100$Est_Pearse_FullSample_q2.5_FirstOnset_onset, p100$AP_SampleSize, mean, na.rm=T)
print("PFull_o")
print(PFull_o.100.Bq025)
PSub_o.100.Bq025 = tapply(p100$Est_Pearse_SubSample_q2.5_FirstOnset_onset, p100$AP_SampleSize, mean, na.rm=T)
print("PSub_o")
print(PSub_o.100.Bq025)

print("q97.5")
Stan.100.Bq975 = tapply(p100$EstTheor_Stan_HMC_q97.5_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print("Stan")
print(Stan.100.Bq975)
PFull.100.Bq975 = tapply(p100$Est_Pearse_FullSample_q97.5_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print("PFull")
print(PFull.100.Bq975)
PSub.100.Bq975 = tapply(p100$Est_Pearse_SubSample_q97.5_FirstOnset, p100$AP_SampleSize, mean, na.rm=T)
print("PSub")
print(PSub.100.Bq975)
PFull_o.100.Bq975 = tapply(p100$Est_Pearse_FullSample_q97.5_FirstOnset_onset, p100$AP_SampleSize, mean, na.rm=T)
print("PFull_o")
print(PFull_o.100.Bq975)
PSub_o.100.Bq975 = tapply(p100$Est_Pearse_SubSample_q97.5_FirstOnset_onset, p100$AP_SampleSize, mean, na.rm=T)
print("PSub_o")
print(PSub_o.100.Bq975)


Obs.10000.m = tapply(p10000$TrueObserved_FirstOnset, p10000$AP_SampleSize, mean, na.rm = TRUE)
Theor.10000.m = tapply(p10000$TheorParam_ExpectedFirstOnset_fromTrueModel, p10000$AP_SampleSize, mean, na.rm=T)
Stan.10000.m = tapply(p10000$EstTheor_Stan_HMC_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PFull.10000.m = tapply(p10000$Est_Pearse_FullSample_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PSub.10000.m = tapply(p10000$Est_Pearse_SubSample_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PFull_o.10000.m = tapply(p10000$Est_Pearse_FullSample_FirstOnset_onset, p10000$AP_SampleSize, mean, na.rm=T)
PSub_o.10000.m = tapply(p10000$Est_Pearse_SubSample_FirstOnset_onset, p10000$AP_SampleSize, mean, na.rm=T)

#Obs.10000.sd = tapply(p10000$TrueObserved_FirstOnset, p10000$AP_SampleSize, sd, na.rm = TRUE)
#Theor.10000.sd = tapply(p10000$TheorParam_ExpectedFirstOnset_fromTrueModel, p10000$AP_SampleSize, sd, na.rm=T)
#Stan.10000.sd = tapply(p10000$EstTheor_Stan_HMC_FirstOnset, p10000$AP_SampleSize, sd, na.rm=T)
#PFull.10000.sd = tapply(p10000$Est_Pearse_FullSample_FirstOnset, p10000$AP_SampleSize, sd, na.rm=T)
#PSub.10000.sd = tapply(p10000$Est_Pearse_SubSample_FirstOnset, p10000$AP_SampleSize, sd, na.rm=T)

Stan.10000.Bq025 = tapply(p10000$EstTheor_Stan_HMC_q2.5_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PFull.10000.Bq025 = tapply(p10000$Est_Pearse_FullSample_q2.5_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PSub.10000.Bq025 = tapply(p10000$Est_Pearse_SubSample_q2.5_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PFull_o.10000.Bq025 = tapply(p10000$Est_Pearse_FullSample_q2.5_FirstOnset_onset, p10000$AP_SampleSize, mean, na.rm=T)
PSub_o.10000.Bq025 = tapply(p10000$Est_Pearse_SubSample_q2.5_FirstOnset_onset, p10000$AP_SampleSize, mean, na.rm=T)

Stan.10000.Bq975 = tapply(p10000$EstTheor_Stan_HMC_q97.5_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PFull.10000.Bq975 = tapply(p10000$Est_Pearse_FullSample_q97.5_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PSub.10000.Bq975 = tapply(p10000$Est_Pearse_SubSample_q97.5_FirstOnset, p10000$AP_SampleSize, mean, na.rm=T)
PFull_o.10000.Bq975 = tapply(p10000$Est_Pearse_FullSample_q97.5_FirstOnset_onset, p10000$AP_SampleSize, mean, na.rm=T)
PSub_o.10000.Bq975 = tapply(p10000$Est_Pearse_SubSample_q97.5_FirstOnset_onset, p10000$AP_SampleSize, mean, na.rm=T)

Obs.1000000.m = tapply(p1000000$TrueObserved_FirstOnset, p1000000$AP_SampleSize, mean, na.rm = TRUE)
Theor.1000000.m = tapply(p1000000$TheorParam_ExpectedFirstOnset_fromTrueModel, p1000000$AP_SampleSize, mean, na.rm=T)
Stan.1000000.m = tapply(p1000000$EstTheor_Stan_HMC_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PFull.1000000.m = tapply(p1000000$Est_Pearse_FullSample_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PSub.1000000.m = tapply(p1000000$Est_Pearse_SubSample_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PFull_o.1000000.m = tapply(p1000000$Est_Pearse_FullSample_FirstOnset_onset, p1000000$AP_SampleSize, mean, na.rm=T)
PSub_o.1000000.m = tapply(p1000000$Est_Pearse_SubSample_FirstOnset_onset, p1000000$AP_SampleSize, mean, na.rm=T)

#Obs.1000000.sd = tapply(p1000000$TrueObserved_FirstOnset, p1000000$AP_SampleSize, sd, na.rm = TRUE)
#Theor.1000000.sd = tapply(p1000000$TheorParam_ExpectedFirstOnset_fromTrueModel, p1000000$AP_SampleSize, sd, na.rm=T)
#Stan.1000000.sd = tapply(p1000000$EstTheor_Stan_HMC_FirstOnset, p1000000$AP_SampleSize, sd, na.rm=T)
#PFull.1000000.sd = tapply(p1000000$Est_Pearse_FullSample_FirstOnset, p1000000$AP_SampleSize, sd, na.rm=T)
#PSub.1000000.sd = tapply(p1000000$Est_Pearse_SubSample_FirstOnset, p1000000$AP_SampleSize, sd, na.rm=T)
#PFull_o.1000000.sd = tapply(p1000000$Est_Pearse_FullSample_FirstOnset_onset, p1000000$AP_SampleSize, sd, na.rm=T)
#PSub_o.1000000.sd = tapply(p1000000$Est_Pearse_SubSample_FirstOnset_onset, p1000000$AP_SampleSize, sd, na.rm=T)

Stan.1000000.Bq025 = tapply(p1000000$EstTheor_Stan_HMC_q2.5_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PFull.1000000.Bq025 = tapply(p1000000$Est_Pearse_FullSample_q2.5_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PSub.1000000.Bq025 = tapply(p1000000$Est_Pearse_SubSample_q2.5_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PFull_o.1000000.Bq025 = tapply(p1000000$Est_Pearse_FullSample_q2.5_FirstOnset_onset, p1000000$AP_SampleSize, mean, na.rm=T)
PSub_o.1000000.Bq025 = tapply(p1000000$Est_Pearse_SubSample_q2.5_FirstOnset_onset, p1000000$AP_SampleSize, mean, na.rm=T)

Stan.1000000.Bq975 = tapply(p1000000$EstTheor_Stan_HMC_q97.5_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PFull.1000000.Bq975 = tapply(p1000000$Est_Pearse_FullSample_q97.5_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PSub.1000000.Bq975 = tapply(p1000000$Est_Pearse_SubSample_q97.5_FirstOnset, p1000000$AP_SampleSize, mean, na.rm=T)
PFull_o.1000000.Bq975 = tapply(p1000000$Est_Pearse_FullSample_q97.5_FirstOnset_onset, p1000000$AP_SampleSize, mean, na.rm=T)
PSub_o.1000000.Bq975 = tapply(p1000000$Est_Pearse_SubSample_q97.5_FirstOnset_onset, p1000000$AP_SampleSize, mean, na.rm=T)

cat("Calculating graphical parameters\n")
ylim100 = range(Obs.100.m,Theor.100.m,Stan.100.m,PFull.100.m,PSub.100.m,Stan.100.Bq025,PFull.100.Bq025,PSub.100.Bq025,Stan.100.Bq975,PFull.100.Bq975,PSub.100.Bq975, PFull_o.100.m, PSub_o.100.m, PFull_o.100.Bq025, PSub_o.100.Bq025, PFull_o.100.Bq975, PSub_o.100.Bq975)
ylim10000 = range(Obs.10000.m,Theor.10000.m,Stan.10000.m,PFull.10000.m,PSub.10000.m,Stan.10000.Bq025,PFull.10000.Bq025,PSub.10000.Bq025,Stan.10000.Bq975,PFull.10000.Bq975,PSub.10000.Bq975, PFull_o.10000.m, PSub_o.10000.m, PFull_o.10000.Bq025, PSub_o.10000.Bq025, PFull_o.10000.Bq975, PSub_o.10000.Bq975)
ylim1000000 = range(Obs.1000000.m,Theor.1000000.m,Stan.1000000.m,PFull.1000000.m,PSub.1000000.m,Stan.1000000.Bq025,PFull.1000000.Bq025,PSub.1000000.Bq025,Stan.1000000.Bq975,PFull.1000000.Bq975,PSub.1000000.Bq975,PFull_o.1000000.m, PSub_o.1000000.m, PFull_o.1000000.Bq025, PSub_o.1000000.Bq025, PFull_o.1000000.Bq975, PSub_o.1000000.Bq975)

print(ylim100)
print(ylim10000)
print(ylim1000000)
ylim = range(ylim100,ylim10000,ylim1000000)

print(ylim)

cat("creating plots\n")
#layout(matrix(c(1, 1, 2, 3, 4, 5), byrow=T, nrow = 3, ncol = 2))
layout(matrix(c(1, 1, 2, 3), byrow=T, nrow = 2, ncol = 2))
par(mar = c(4, 4, 2, 1))  # (bottom, left, top, right)

#from viridis palette
#col100.orig = rgb(0, 114/255, 178/255)
#col100T.orig = rgb(0, 114/255, 178/255, 0.1)
#col10000.orig = rgb(230/255, 159/255, 0)
#col10000T.orig = rgb(230/255, 159/255, 0, 0.1)
#col1000000.orig = rgb(213/255, 94/255, 0)
#col1000000T.orig = rgb(213/255, 94/255, 0)

#Okabe-Ito palette
col100 = rgb(230/255, 159/255, 0/255)
col100T = rgb(230/255, 159/255, 0/255, 0.1)
col10000 = rgb(0/255, 158/255, 115/255)
col10000T = rgb(0/255, 158/255, 115/255, 0.1)
col1000000 = rgb(204/255, 121/255, 167/255)
col1000000T = rgb(204/255, 121/255, 167/255, 0.1)

#rcartocolors Safe
#col100 = rgb(136/255, 204/255, 238/255)
#col100T = rgb(136/255, 204/255, 238/255, 0.1)
#col10000 = rgb(204/255, 102/255, 119/255)
#col10000T = rgb(204/255, 102/255, 119/255, 0.1)
#col1000000 = rgb(136/255, 136/255, 136/255)
#col1000000T = rgb(136/255, 136/255, 136/255, 0.1)

#viridis inferno
#col100 = rgb(75/255, 12/255, 107/255)
#col100T = rgb(75/255, 12/255, 107/255, 0.1)
#col10000 = rgb(184/255, 54/255, 85/255)
#col10000T = rgb(184/255, 54/255, 85/255, 0.1)
#col1000000 = rgb(253/255, 231/255, 37/255)
#col1000000T = rgb(253/255, 231/255, 37/255, 0.1)

#inferno 2
#col100 = rgb(48/255, 10/255, 180/255)
#col100T = rgb(48/255, 10/255, 180/255, 0.1)
#col10000 = rgb(177/255, 50/255, 90/255)
#col10000T = rgb(177/255, 50/255, 90/255, 0.1)
#col1000000 = rgb(252/255, 160/255, 7/255)
#col1000000T = rgb(252/255, 160/255, 7/255, 0.1)

#Plot of Stan inferences
plot(x, Theor.100.m, ylim=ylim, type='l', lwd=5, col="black", xlab="Sample Size", ylab="Estimate of Population First Onset (Day of Year)")	#Theoretical value based on true parameters
points(x, Obs.100.m, type='l', col=col100, lwd=3)			#Observed value based on simulated population of 100 individuals
points(x, Stan.100.m, type='l', lty=3, col=col100, lwd=3)		#Theoretical value based on Stan estimates of parameters
polygon(							#Theoretical 95% CI based on Stan estimates of parameters
  c(x, rev(x)),              
  c(Stan.100.Bq975, rev(Stan.100.Bq025)),      
  col = col100T,  
  border = NA                
)

points(x, Theor.10000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
points(x, Obs.10000.m, type='l', col=col10000, lwd=3)			#Observed value based on simulated population of 10000 individuals
points(x, Stan.10000.m, type='l', lty=3, col=col10000, lwd=3)		#Theoretical value based on Stan estimates of parameters
polygon(							#Theoretical 95% CI based on Stan estimates of parameters
  c(x, rev(x)),              
  c(Stan.10000.Bq975, rev(Stan.10000.Bq025)),      
  col = col10000T,  
  border = NA                
)

points(x, Theor.1000000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
points(x, Obs.1000000.m, type='l', col=col1000000, lwd=3)			#Observed value based on simulated population of 1000000 individuals
points(x, Stan.1000000.m, type='l', lty=3, col=col1000000, lwd=3)		#Theoretical value based on Stan estimates of parameters
polygon(							#Theoretical 95% CI based on Stan estimates of parameters
  c(x, rev(x)),              
  c(Stan.1000000.Bq975, rev(Stan.1000000.Bq025)),      
  col = col1000000T,  
  border = NA                
)

mtext("A", side = 1, line = 3, adj=0, cex = 1.5)  # Label "a" on the bottom left

legend("bottomright",
       legend = c("100", "10,000", "1,000,000"),
       fill = c(col100, col10000, col1000000),
       border = "black",
       cex = 1.2,
       bty = "n",
       title = "Population Size")


#Plot of Pearse at Stan's sample size
plot(x, Theor.100.m, ylim=ylim, type='l', lwd=5, col="black", xlab="k Value (= Sample Size)", ylab="Estimate of Population First Onset (Day of Year)")	#Theoretical value based on true parameters
points(x, Obs.100.m, type='l', col=col100, lwd=3)			#Observed value based on simulated population of 100 individuals
points(x, PSub.100.m, type='l', lty=3, col=col100, lwd=3)		#Theoretical value based on PSub estimates of parameters
polygon(							#Theoretical 95% CI based on PSub estimates of parameters
  c(x, rev(x)),              
  c(PSub.100.Bq975, rev(PSub.100.Bq025)),      
  col = col100T,  
  border = NA                
)

points(x, Theor.10000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
points(x, Obs.10000.m, type='l', col=col10000, lwd=3)			#Observed value based on simulated population of 10000 individuals
points(x, PSub.10000.m, type='l', lty=3, col=col10000, lwd=3)		#Theoretical value based on PSub estimates of parameters
polygon(							#Theoretical 95% CI based on PSub estimates of parameters
  c(x, rev(x)),              
  c(PSub.10000.Bq975, rev(PSub.10000.Bq025)),      
  col = col10000T,  
  border = NA                
)

points(x, Theor.1000000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
points(x, Obs.1000000.m, type='l', col=col1000000,lwd=3)			#Observed value based on simulated population of 1000000 individuals
points(x, PSub.1000000.m, type='l', lty=3, col=col1000000,lwd=3)		#Theoretical value based on PSub estimates of parameters
polygon(							#Theoretical 95% CI based on PSub estimates of parameters
  c(x, rev(x)),              
  c(PSub.1000000.Bq975, rev(PSub.1000000.Bq025)),      
  col = col1000000T,  
  border = NA                
)

mtext("B", side = 1, line = 3, adj=0, cex = 1.5)  # Label "b" on the bottom left

#Plot of Pearse estimates provided the full population sample
plot(x, Theor.100.m, ylim=ylim, type='l', lwd=5, col="black", xlab="k Value (Full Population Sample)", ylab="Estimate of Population First Onset (Day of Year)")	#Theoretical value based on true parameters
points(x, Obs.100.m, type='l', col=col100, lwd=3)			#Observed value based on simulated population of 100 individuals
points(x, PFull.100.m, type='l', lty=3, col=col100, lwd=3)		#Theoretical value based on PFull estimates of parameters
polygon(							#Theoretical 95% CI based on PFull estimates of parameters
  c(x, rev(x)),              
  c(PFull.100.Bq975, rev(PFull.100.Bq025)),      
  col = col100T,  
  border = NA                
)

points(x, Theor.10000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
points(x, Obs.10000.m, type='l', col=col10000, lwd=3)			#Observed value based on simulated population of 10000 individuals
points(x, PFull.10000.m, type='l', lty=3, col=col10000, lwd=3)		#Theoretical value based on PFull estimates of parameters
polygon(							#Theoretical 95% CI based on PFull estimates of parameters
  c(x, rev(x)),              
  c(PFull.10000.Bq975, rev(PFull.10000.Bq025)),      
  col = col10000T,  
  border = NA                
)

points(x, Theor.1000000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
points(x, Obs.1000000.m, type='l', col=col1000000,lwd=3)			#Observed value based on simulated population of 1000000 individuals
points(x, PFull.1000000.m, type='l', lty=3, col=col1000000,lwd=3)		#Theoretical value based on PFull estimates of parameters
polygon(							#Theoretical 95% CI based on PFull estimates of parameters
  c(x, rev(x)),              
  c(PFull.1000000.Bq975, rev(PFull.1000000.Bq025)),      
  col = col1000000T,  
  border = NA                
)

mtext("C", side = 1, line = 3, adj=0, cex = 1.5)  # Label "c" on the bottom left

##Plot of Pearse at Stan's sample size
#plot(x, Theor.100.m, ylim=ylim, type='l', lwd=5, col="black", xlab="k Value (= Sample Size, Onset Values Sampled)", ylab="Estimate of Population First Onset (Day of Year)")	#Theoretical value based on true parameters
#points(x, Obs.100.m, type='l', col=rgb(0, 114/255, 178/255), lwd=3)			#Observed value based on simulated population of 100 individuals
#points(x, PSub_o.100.m, type='l', lty=3, col=rgb(0, 114/255, 178/255), lwd=3)		#Theoretical value based on PSub estimates of parameters
#polygon(							#Theoretical 95% CI based on PSub estimates of parameters
  #c(x, rev(x)),              
  #c(PSub_o.100.Bq975, rev(PSub_o.100.Bq025)),      
  #col = rgb(0, 114/255, 178/255, 0.1),  
  #border = NA                
#)
#
#points(x, Theor.10000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
#points(x, Obs.10000.m, type='l', col=rgb(230/255, 159/255, 0), lwd=3)			#Observed value based on simulated population of 10000 individuals
#points(x, PSub_o.10000.m, type='l', lty=3, col=rgb(230/255, 159/255, 0), lwd=3)		#Theoretical value based on PSub estimates of parameters
#polygon(							#Theoretical 95% CI based on PSub estimates of parameters
  #c(x, rev(x)),              
  #c(PSub_o.10000.Bq975, rev(PSub_o.10000.Bq025)),      
  #col = rgb(230/255, 159/255, 0, 0.1),  
  #border = NA                
#)
#
#points(x, Theor.1000000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
#points(x, Obs.1000000.m, type='l', col=rgb(213/255, 94/255, 0),lwd=3)			#Observed value based on simulated population of 1000000 individuals
#points(x, PSub_o.1000000.m, type='l', lty=3, col=rgb(213/255, 94/255, 0),lwd=3)		#Theoretical value based on PSub estimates of parameters
#polygon(							#Theoretical 95% CI based on PSub estimates of parameters
  #c(x, rev(x)),              
  #c(PSub_o.1000000.Bq975, rev(PSub_o.1000000.Bq025)),      
  #col = rgb(213/255, 94/255, 0, 0.1),  
  #border = NA                
#)
#
#mtext("D", side = 1, line = 3, adj=0, cex = 1.5)  # Label "D" on the bottom left
#
##Plot of Pearse estimates provided the full population sample
#plot(x, Theor.100.m, ylim=ylim, type='l', lwd=5, col="black", xlab="k Value (Full Population Sample, Onset Values Sampled)", ylab="Estimate of Population First Onset (Day of Year)")	#Theoretical value based on true parameters
#points(x, Obs.100.m, type='l', col=rgb(0, 114/255, 178/255), lwd=3)			#Observed value based on simulated population of 100 individuals
#points(x, PFull_o.100.m, type='l', lty=3, col=rgb(0, 114/255, 178/255), lwd=3)		#Theoretical value based on PFull estimates of parameters
#polygon(							#Theoretical 95% CI based on PFull estimates of parameters
  #c(x, rev(x)),              
  #c(PFull_o.100.Bq975, rev(PFull_o.100.Bq025)),      
  #col = rgb(0, 114/255, 178/255, 0.1),  
  #border = NA                
#)
#
#points(x, Theor.10000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
#points(x, Obs.10000.m, type='l', col=rgb(230/255, 159/255, 0), lwd=3)			#Observed value based on simulated population of 10000 individuals
#points(x, PFull_o.10000.m, type='l', lty=3, col=rgb(230/255, 159/255, 0), lwd=3)		#Theoretical value based on PFull estimates of parameters
#polygon(							#Theoretical 95% CI based on PFull estimates of parameters
  #c(x, rev(x)),              
  #c(PFull_o.10000.Bq975, rev(PFull_o.10000.Bq025)),      
  #col = rgb(230/255, 159/255, 0, 0.1),  
  #border = NA                
#)
#
#points(x, Theor.1000000.m, ylim=ylim, type='l', lwd=5, col="black")	#Theoretical value based on true parameters
#points(x, Obs.1000000.m, type='l', col=rgb(213/255, 94/255, 0),lwd=3)			#Observed value based on simulated population of 1000000 individuals
#points(x, PFull_o.1000000.m, type='l', lty=3, col=rgb(213/255, 94/255, 0),lwd=3)		#Theoretical value based on PFull estimates of parameters
#polygon(							#Theoretical 95% CI based on PFull estimates of parameters
  #c(x, rev(x)),              
  #c(PFull_o.1000000.Bq975, rev(PFull_o.1000000.Bq025)),      
  #col = rgb(213/255, 94/255, 0, 0.1),  
  #border = NA                
#)
#
#mtext("E", side = 1, line = 3, adj=0, cex = 1.5)  # Label "E" on the bottom left
