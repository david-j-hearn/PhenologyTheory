library(ggplot2)
library(cowplot)

data = read.table("results.CFq.txt", header=T, sep='\t')
data_wBias = data[ data$TrueParam_MeanOnset != data$TrueHyper_mean_MeanOnset, ]

data_wBias <- na.omit(data_wBias)
data_BB = data[ data$AP_SimulationModel == "BB", ]
data_GP = data[ data$AP_SimulationModel == "GP", ]

data_wBias$err.O = data_wBias$TrueParam_MeanOnset - data_wBias$Est_Stan_HMC_MeanOnset
data_wBias$err.D = data_wBias$TrueParam_MeanDuration - data_wBias$Est_Stan_HMC_MeanDuration
data_wBias$err.C = data_wBias$TrueParam_MeanCessation - data_wBias$Est_Stan_HMC_MeanCessation
data_wBias$err.S = data_wBias$TrueParam_SDOnset - data_wBias$Est_Stan_HMC_Sigma
data_wBias$err.O1 = data_wBias$TheorParam_ExpectedFirstOnset_fromTrueModel - data_wBias$EstTheor_Stan_HMC_FirstOnset
data_wBias$err.P = data_wBias$TheorParam_PeakPhenophase_fromTrueModel - data_wBias$EstTheor_Stan_HMC_PeakPhenophase

set.seed(42)
#simulation of 1000 replicates, as reported in MS, including models with hyperparameter bias
data_wBias = data_wBias[sample(nrow(data_wBias), 1000), ]


# Define layout: 1 row, 3 columns
#layout(matrix(c(1,2,3,4), nrow = 2, ncol = 2))  # layout matrix for 3 side-by-side plots

# Optional: set margins
#par(mar = c(4, 4, 2, 1))  # bottom, left, top, right margins


# Prepare data
df <- data.frame(
  value = data_wBias$err.O,
  group = data_wBias$AP_SimulationModel
)

print(df)


print("onset")
print(t.test(abs(value) ~ group, data=df))
print(summary(aov(abs(value) ~ group, data=df)))
print(aggregate(abs(value) ~ group, data=df, FUN=mean))
print(aggregate((value) ~ group, data=df, FUN=mean))

# Plot
p4 = ggplot(df, aes(x = group, y = value)) +
  geom_violin(fill = rgb(1,0,0,0.5)) +
  theme_minimal() +
  labs(title = NULL, x = "Model", y = "Raw Error on Mean Onset")

df <- data.frame(
  value = data_wBias$err.D,
  group = data_wBias$AP_SimulationModel
)
print("duration")
print(t.test(abs(value) ~ group, data=df))
print(summary(aov(abs(value) ~ group, data=df)))
print(aggregate(abs(value) ~ group, data=df, FUN=mean))
print(aggregate((value) ~ group, data=df, FUN=mean))

# Plot
p1 = ggplot(df, aes(x = group, y = value)) +
  geom_violin(fill = rgb(0,0,0,0.5)) +
  theme_minimal() +
  labs(title = NULL, x = "Model", y = "Raw Error on Mean Duration")

df <- data.frame(
  value = data_wBias$err.C,
  group = data_wBias$AP_SimulationModel
)
print("cessation")
print(t.test(abs(value) ~ group, data=df))
print(summary(aov(abs(value) ~ group, data=df)))
print(aggregate(abs(value) ~ group, data=df, FUN=mean))
print(aggregate((value) ~ group, data=df, FUN=mean))

# Plot
p6 = ggplot(df, aes(x = group, y = value)) +
  geom_violin(fill = rgb(0,0,1,0.5)) +
  theme_minimal() +
  labs(title = NULL, x = "Model", y = "Raw Error on Mean Cessation")

df <- data.frame(
  value = data_wBias$err.S,
  group = data_wBias$AP_SimulationModel
)
print("sigma")
print(t.test(abs(value) ~ group, data=df))
print(summary(aov(abs(value) ~ group, data=df)))
print(aggregate(abs(value) ~ group, data=df, FUN=mean))
print(aggregate((value) ~ group, data=df, FUN=mean))

# Plot
p2 = ggplot(df, aes(x = group, y = value)) +
  geom_violin(fill = rgb(135/255,206/255,235/255,0.5)) +
  theme_minimal() +
  labs(title = NULL, x = "Model", y = "Raw Error on Sigma for Onset")

df <- data.frame(
  value = data_wBias$err.O1,
  group = data_wBias$AP_SimulationModel
)
print("first onset")
print(t.test(abs(value) ~ group, data=df))
print(summary(aov(abs(value) ~ group, data=df)))
print(aggregate(abs(value) ~ group, data=df, FUN=mean))
print(aggregate((value) ~ group, data=df, FUN=mean))

# Plot
p3 = ggplot(df, aes(x = group, y = value)) +
  geom_violin(fill = rgb(1,1,0, 0.5)) +
  theme_minimal() +
  labs(title = NULL, x = "Model", y = "Raw Error on First Onset")

df <- data.frame(
  value = data_wBias$err.P,
  group = data_wBias$AP_SimulationModel
)

print("peak")
print(t.test(abs(value) ~ group, data=df))
print(summary(aov(abs(value) ~ group, data=df)))
print(aggregate(abs(value) ~ group, data=df, FUN=mean))
print(aggregate((value) ~ group, data=df, FUN=mean))

# Plot
p5 = ggplot(df, aes(x = group, y = value)) +
  geom_violin(fill = rgb(1,0,1,0.5)) +
  theme_minimal() +
  labs(title = NULL, x = "Model", y = "Raw Error on Peak Phenophase")

print(plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, labels = c("A", "B", "C", "D", "E", "F"), label_x = 0, label_y = 0.1))
