source("phenologyDistributions.R")

visualizeSimulatedData_GP = function(sim, nbreaks=100, col=c(rgb(1,0,0,0.5),rgb(1,0,1,0.5),rgb(0,0,1,0.5)),min=0, max=365) {
	xmin = ifelse(min(sim$t_start) < min - 0.1*(max-min), min(sim$t_start), min - 0.2*(max-min))
	xmax = ifelse(max(sim$t_end) > max + 0.1*(max-min), max(sim$t_end), max + 0.2*(max-min))
	curve(dfirst_start_time_GP(x,N=sim$n, mu_O=sim$onset_mean, sigma=sim$sigma), xlim=c(xmin, xmax),n=1000)
	curve(dstart_time_GP(x,mu_O=sim$onset_mean, sigma=sim$sigma), add=T,n=1000)
	curve(dobserved_GP(x,mean_s=sim$onset_mean, mean_e=sim$cessation_mean, sigma=sim$sigma), add=T,n=1000)
	curve(dend_time_GP(x,mu_C=sim$cessation_mean, sigma=sim$sigma), add=T,n=1000)
	curve(dlast_end_time_GP(x,N=sim$n,mu_C=sim$cessation_mean, sigma=sim$sigma), add=T,n=1000)
	abline(v=c(sim$onset_mean, sim$observed_mean, sim$cessation_mean, sim$e_last_end_time[1], sim$e_first_start_time[1]))

	hist(sim$t_start, breaks=seq(min,max,length.out=nbreaks), col=col[1], prob=T, add=T)
	hist(sim$observed, breaks=seq(min,max,length.out=nbreaks), col=col[2], add=T, prob=T)
	hist(sim$t_end, breaks=seq(min,max,length.out=nbreaks), col=col[3], add=T, prob=T)
}

do_PosteriorComparisonToSimulated_GP = function(posteriorResults,sim,nbreaks=25, min=0, max=365) {
	par = posteriorResults$par

	#print("hists")

	sim$t_start = (sim$t_start - min) / (max - min)
	sim$t_end = (sim$t_end - min) / (max - min)
	sim$observed = (sim$observed - min) / (max - min)
	sim$onset_mean = (sim$onset_mean - min) / (max - min)
	sim$cessation_mean = (sim$cessation_mean - min) / (max - min)
	sim$sigma = (sim$sigma) / (max - min)


hist(sim$t_start, breaks = seq(0,1,length.out=nbreaks), xlim=c(0,1), col = rgb(1,0,0,0.2), prob=T )
hist(sim$observed, breaks = seq(0,1,length.out=nbreaks), col = rgb(1,0,1,0.5), prob=T, add=T)
hist(sim$t_end, breaks = seq(0,1,length.out=nbreaks), col = rgb(0,0,1,0.2), prob=T, add=T)

res = 1000
	#print("observed")
curve(dobserved(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dobserved_GP(x, sim$onset_mean, sim$cessation_mean, sim$sigma),
      add = TRUE, col = "purple", lwd = 1, n=res)

#onset
	#print("onset")
curve(dstart_time(x, par[1], par[2]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dstart_time_GP(x, sim$onset_mean, sim$sigma),
      add = TRUE, col = "red", lwd = 1, n=res)

#cessation
	#print("cessation")
curve(dend_time(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dend_time_GP(x, sim$cessation_mean, sim$sigma),
      add = TRUE, col = "blue", lwd = 1, n=res)
	abline(v=c(sim$onset_mean, (sim$observed_mean-1)/(max-min), sim$cessation_mean), lwd=2)

	#print("stats")
MAP_mean_s = beta_mean(par[1], par[2])
MAP_sd_s = beta_sd(par[1], par[2])*(max-min)
MAP_mean_d = beta_mean(par[3], par[4]) * (1 - MAP_mean_s) * (max-min)
MAP_mean_s = min + MAP_mean_s * (max - min)
MAP_sd_d = beta_sd(par[3], par[3])*(max-min)
	abline(v=c((MAP_mean_s-min)/(max-min), expected_observed(par[1],par[2],par[3],par[4]), (MAP_mean_s + MAP_mean_d - min)/(max-min)), col=c("red","purple","blue"))

#This DOESN'T TAKE INTO ACCOUNT THE CONTRACTION in durations due to the non-independence with onset
onset_mean = min + sim$onset_mean * (max - min)
onset_sd = sim$sigma * (max - min)
duration_mean = sim$duration_mean
duration_sd = 0 

comparison_table <- data.frame(
  Parameter = c("Start time mean", "Start time sd", "Duration mean", "Duration sd"),
  True_Value = c(onset_mean, onset_sd, duration_mean, duration_sd),
  Estimated_Value = c(MAP_mean_s, MAP_sd_s, MAP_mean_d, MAP_sd_d)
)

print(comparison_table)
}

# --- 9. Overlay fitted density on histogram ---
do_PosteriorComparisonToSimulated = function(posteriorResults,sim,nbreaks=25, min=0, max=365) {
	par = posteriorResults$par

hist(sim$t_start, breaks = seq(0,1,length.out=nbreaks), xlim=c(0,1), col = rgb(1,0,0,0.2), prob=T )
hist(sim$observed, breaks = seq(0,1,length.out=nbreaks), col = rgb(1,0,1,0.5), prob=T, add=T)
hist(sim$t_end, breaks = seq(0,1,length.out=nbreaks), col = rgb(0,0,1,0.2), prob=T, add=T)

res = 1000
curve(dobserved(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dobserved(x, sim$alpha_s, sim$beta_s,
                        sim$alpha_d, sim$beta_d),
      add = TRUE, col = "purple", lwd = 2, n=res)

#onset
curve(dstart_time(x, par[1], par[2]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dstart_time(x, sim$alpha_s, sim$beta_s),
      add = TRUE, col = "red", lwd = 2, n=res)

#cessation
curve(dend_time(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dend_time(x, sim$alpha_s, sim$beta_s, sim$alpha_d, sim$beta_d),
      add = TRUE, col = "blue", lwd = 2, n=res)

MAP_mean_s = min + beta_mean(par[1], par[2])*(max-min)
MAP_sd_s = beta_sd(par[1], par[2])*(max-min)
MAP_mean_d = beta_mean(par[3], par[4])*(max-min)
MAP_sd_d = beta_sd(par[3], par[3])*(max-min)

#This DOESN'T TAKE INTO ACCOUNT THE CONTRACTION in durations due to the non-independence with onset
onset_mean = min + beta_mean(sim$alpha_s, sim$beta_s)*(max-min)
onset_sd = beta_sd(sim$alpha_s, sim$beta_s)*(max-min)
duration_mean = beta_mean(sim$alpha_d, sim$beta_d) *(max-min)
duration_sd = beta_sd(sim$alpha_d, sim$beta_d) *(max-min)

comparison_table <- data.frame(
  Parameter = c("Start time mean", "Start time sd", "Duration mean", "Duration sd"),
  True_Value = c(onset_mean, onset_sd, duration_mean, duration_sd),
  Estimated_Value = c(MAP_mean_s, MAP_sd_s, MAP_mean_d, MAP_sd_d)
)

print(comparison_table)
}

doFitDistComparisonToSimulated = function(fit,sim,nbreaks=25) {
hist(sim$t_start/365, breaks = seq(0,1,length.out=nbreaks), xlim=c(0,1), col = rgb(1,0,0,0.2), prob=T )
hist(sim$observed/365, breaks = seq(0,1,length.out=nbreaks), col = "purple", prob=T, main = "Fit of observed", xlab = "Time", add=T)
hist(sim$t_end/365, breaks = seq(0,1,length.out=nbreaks), col = rgb(0,0,1,0.2), prob=T, add=T)

res = 1000
curve(dobserved(x, fit$estimate["alpha_s"], fit$estimate["beta_s"],
                        fit$estimate["alpha_d"], fit$estimate["beta_d"]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dobserved(x, sim$alpha_s, sim$beta_s,
                        sim$alpha_d, sim$beta_d),
      add = TRUE, col = "purple", lwd = 2, n=res)

#onset
curve(dstart_time(x, fit$estimate["alpha_s"], fit$estimate["beta_s"]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dstart_time(x, sim$alpha_s, sim$beta_s),
      add = TRUE, col = "red", lwd = 2, n=res)

#cessation
curve(dend_time(x, fit$estimate["alpha_s"], fit$estimate["beta_s"],
                        fit$estimate["alpha_d"], fit$estimate["beta_d"]),
      add = TRUE, col = "black", lwd = 2, n=res)
curve(dend_time(x, sim$alpha_s, sim$beta_s, sim$alpha_d, sim$beta_d),
      add = TRUE, col = "blue", lwd = 2, n=res)

MLE_mean_s = beta_mean(fit$estimate["alpha_s"], fit$estimate["beta_s"])
MLE_sd_s = beta_sd(fit$estimate["alpha_s"], fit$estimate["beta_s"])
MLE_mean_d = beta_mean(fit$estimate["alpha_d"], fit$estimate["beta_d"])
MLE_sd_d = beta_sd(fit$estimate["alpha_d"], fit$estimate["beta_d"])

MLE_mean_s_SE = compute_se(fit$estimate["alpha_s"], fit$estimate["beta_s"], fit$sd["alpha_s"], fit$sd["beta_s"])$se_mean
MLE_sd_s_SE = compute_se(fit$estimate["alpha_s"], fit$estimate["beta_s"], fit$sd["alpha_s"], fit$sd["beta_s"])$se_sd
MLE_mean_d_SE = compute_se(fit$estimate["alpha_d"], fit$estimate["beta_d"], fit$sd["alpha_d"], fit$sd["beta_d"])$se_mean
MLE_sd_d_SE = compute_se(fit$estimate["alpha_d"], fit$estimate["beta_d"], fit$sd["alpha_d"], fit$sd["beta_d"])$se_sd

#This DOESN'T TAKE INTO ACCOUNT THE CONTRACTION in durations due to the non-independence with onset
onset_mean = beta_mean(sim$alpha_s, sim$beta_s)
onset_sd = beta_sd(sim$alpha_s, sim$beta_s)
duration_mean = beta_mean(sim$alpha_d, sim$beta_d) 
duration_sd = beta_sd(sim$alpha_d, sim$beta_d) 

comparison_table <- data.frame(
  Parameter = c("Start time mean", "Start time sd", "Duration mean", "Duration sd"),
  True_Value = c(onset_mean, onset_sd, duration_mean, duration_sd),
  Estimated_Value = c(MLE_mean_s, MLE_sd_s, MLE_mean_d, MLE_sd_d),
  Estimated_Value_SE = c(MLE_mean_s_SE, MLE_sd_s_SE, MLE_mean_d_SE, MLE_sd_d_SE),
  Parameter_q2.5 = c(MLE_mean_s - 1.96*MLE_mean_s_SE, MLE_sd_s - 1.96*MLE_sd_s_SE, MLE_mean_d - 1.96*MLE_mean_d_SE, MLE_sd_d - 1.96*MLE_sd_d_SE),
  Parameter_q97.5 = c(MLE_mean_s + 1.96*MLE_mean_s_SE, MLE_sd_s + 1.96*MLE_sd_s_SE, MLE_mean_d + 1.96*MLE_mean_d_SE, MLE_sd_d + 1.96*MLE_sd_d_SE)
)

print(comparison_table)
}


do_PosteriorComparisonToEmpirical = function(posteriorResults,observed,nbreaks=25,min=0,max=365) {
	par = posteriorResults$par

hist(observed, breaks = seq(0,1,length.out=nbreaks), xlim=c(0,1), ylim=c(0,20), col = rgb(1,0,0,0.2), prob=T )

res = 1000
curve(dobserved(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)

#onset
curve(dstart_time(x, par[1], par[2]),
      add = TRUE, col = "black", lwd = 2, n=res)

#cessation
curve(dend_time(x, par[1], par[2],
                        par[3], par[4]),
      add = TRUE, col = "black", lwd = 2, n=res)

MAP_mean_s = beta_mean(par[1], par[2])*365
MAP_sd_s = beta_sd(par[1], par[2])*365
MAP_mean_d = beta_mean(par[3], par[4])*365
MAP_sd_d = beta_sd(par[3], par[3])*365

print(paste("Start: ", MAP_mean_s, "(+/-", MAP_sd_s, " 1 SD) Duration: ", MAP_mean_d, "(+/-", MAP_sd_d, " 1 SD)"))
}

doFitDistComparisonToEmpirical = function(fit,observed,nbreaks=25) {
hist(observed, breaks = seq(0,1,length.out=nbreaks), col = "purple", prob=T, main = "Fit of observed", xlab = "Time")

res = 1000
curve(dobserved(x, fit$estimate["alpha_s"], fit$estimate["beta_s"],
                        fit$estimate["alpha_d"], fit$estimate["beta_d"]),
      add = TRUE, col = "red", lwd = 2, n=res)

#onset
curve(dstart_time(x, fit$estimate["alpha_s"], fit$estimate["beta_s"]),
      add = TRUE, col = "black", lwd = 2, n=res)

#cessation
curve(dend_time(x, fit$estimate["alpha_s"], fit$estimate["beta_s"],
                        fit$estimate["alpha_d"], fit$estimate["beta_d"]),
      add = TRUE, col = "black", lwd = 2, n=res)

MLE_mean_s = beta_mean(fit$estimate["alpha_s"], fit$estimate["beta_s"])
MLE_sd_s = beta_sd(fit$estimate["alpha_s"], fit$estimate["beta_s"])
MLE_mean_d = beta_mean(fit$estimate["alpha_d"], fit$estimate["beta_d"])
MLE_sd_d = beta_sd(fit$estimate["alpha_d"], fit$estimate["beta_d"])

MLE_mean_s_SE = compute_se(fit$estimate["alpha_s"], fit$estimate["beta_s"], fit$sd["alpha_s"], fit$sd["beta_s"])$se_mean
MLE_sd_s_SE = compute_se(fit$estimate["alpha_s"], fit$estimate["beta_s"], fit$sd["alpha_s"], fit$sd["beta_s"])$se_sd
MLE_mean_d_SE = compute_se(fit$estimate["alpha_d"], fit$estimate["beta_d"], fit$sd["alpha_d"], fit$sd["beta_d"])$se_mean
MLE_sd_d_SE = compute_se(fit$estimate["alpha_d"], fit$estimate["beta_d"], fit$sd["alpha_d"], fit$sd["beta_d"])$se_sd

print(paste("Start: ", MLE_mean_s, "(+/-", MLE_sd_s, " 1 SD) Duration: ", MLE_mean_d, "(+/-", MLE_sd_d, " 1 SD)"))
}

plotBivariateContColor = function(x,y,colorVar) {
        library(ggplot2)
        df <- data.frame(
        x = x,
        y = y,
        z = colorVar
        )
# Plot with continuous color scale
ggplot(df, aes(x = x, y = y, color = z)) +
  geom_point(size = 1) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()
}

plotBivariateDiscColor = function(x,y,colorFactVar,x_axis = "Mean Duration", y_axis = "Raw Error", color="Collapsed", legend=T, yesCol = "red", noCol = "black") {
library(ggplot2)
df <- data.frame(
  x = x,
  y = y,
  group = as.factor(colorFactVar)
)

# Plot with automatic color mapping
if(legend) {
p = ggplot(df, aes(x = x, y = y, color = group)) +
  scale_color_manual(values = c("No" = noCol, "Yes" = yesCol)) +
  geom_point(size = 1)  +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3, inherit.aes = FALSE, aes(x = x, y = y)) +
  labs(x = x_axis, y = y_axis, color = color) +
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 10))  # Top, Right, Bottom, Left
  #theme_minimal()
}
else {
p = ggplot(df, aes(x = x, y = y, color = group)) +
  scale_color_manual(values = c("No" = "black", "Yes" = "red"), guide="none") +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3, inherit.aes = FALSE, aes(x = x, y = y))+
  labs(x = x_axis, y = y_axis, color = color)+
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 10))  # Top, Right, Bottom, Left
  #theme_minimal()
}

return(p)
}

plotBivariateDiscColor_wHighlight = function(x,y,colorFactVar,x_axis = "Mean Duration", y_axis = "Raw Error", color="Collapsed", highlight = 1, legend=TRUE) {
library(ggplot2)
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
 geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3, inherit.aes = FALSE, aes(x = x, y = y)) +

  geom_point(data = df[highlight, ], aes(x, y), color = "yellow", size = 4) +
  labs(x = x_axis, y = y_axis, color = color)+
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 10))  # Top, Right, Bottom, Left
  #theme_minimal()
}
else {
p = ggplot(df, aes(x = x, y = y, color = group)) +
  scale_color_manual(values = c("No" = "black", "Yes" = "red"), guide="none") +
  geom_point(size = 1) +
   geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3, inherit.aes = FALSE, aes(x = x, y = y)) +
  geom_point(data = df[highlight, ], aes(x, y), color = "yellow", size = 4) +
  labs(x = x_axis, y = y_axis, color = color)+
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 10))  # Top, Right, Bottom, Left
  #theme_minimal()
}

return(p)
}
plotPointsWithErrorBars = function(x, est_val, true_val, true_exp_val, q025, q975, contColVar, colVarMid=50, trueCol="red", estCol="blue", trueExpCol = "green", sizeTrue=1, sizeEst=1, xlab, ylab, makeLegend=T) {
        library(ggplot2)
df <- data.frame(
  x = x,
  est = est_val,
  true = true_val,
  trueExp = true_exp_val,
  lower = q025,
  upper = q975,
  color_value = contColVar
)
if(!makeLegend) {
ggplot(df, aes(x = x)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = color_value), width = 0.0) +
  geom_point(aes(y = est), color = estCol, size = sizeEst) +
  geom_point(aes(y = true), shape = 17, size = sizeTrue, color = trueCol) +
  geom_point(aes(y = trueExp), shape = 10, size = sizeTrue, color = trueExpCol) +
  scale_color_gradient2(
    low = "blue",     # Low end
    mid = "black",    # Middle
    high = "red",     # High end
    midpoint = colVarMid      # Center of gradient
  ) +
  #ylim(-20, max(df$upper)) + 
  guides(color = "none") +
  theme_minimal() +
  labs(x = xlab, y = ylab, color = "Gradient Variable")
}
else {
ggplot(df, aes(x = x)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = color_value), width = 0.0) +
  geom_point(aes(y = est), color = estCol, size = sizeEst) +
  geom_point(aes(y = true), shape = 17, size = sizeTrue, color = trueCol) +
  geom_point(aes(y = trueExp), shape = 10, size = sizeTrue, color = trueExpCol) +
  scale_color_gradient2(
    low = "blue",     # Low end
    mid = "black",    # Middle
    high = "red",     # High end
    midpoint = colVarMid      # Center of gradient
  ) +
  #ylim(-20, max(df$upper)) + 
  theme_minimal() +
  labs(x = xlab, y = ylab, color = "Gradient Variable")
}
}

