library(phenoCollectR)
# Define the time range for plotting
t_vals = seq(90, 250, length.out = 365) # Adjust to your time range

# Parameters
pop_sizes = c(10,100, 1000, 10000, 100000)
muO = 150
sdO = 10
muD = 90
sdD = 30

# Compute densities
dT_vals = dT(x=t_vals, mu_O = muO, sigma_O=sdO, mu_D=muD,sigma_D=sdD,minResponse=0, maxResponse=365,type="BB")
dO1_vals = sapply(pop_sizes, function(N) dOk1(x=t_vals, mu_O = muO, sigma_O=sdO,minResponse=0, maxResponse=365,type="BB", N=N)) # First onset densities

# --- Plot --- #
# Start empty plot with collection time density limits
plot(t_vals, dT_vals, type = "n", ylim = c(0, max(c(dT_vals, dO1_vals))), xlab = "Day of Year", ylab = "Density", main = NULL)

# Add shaded collection time density (purple)
col_T = rgb(1, 0, 1, 0.3)
polygon(c(t_vals, rev(t_vals)), c(dT_vals, rep(0, length(dT_vals))), col = col_T, border = NA)

col_O1 = viridisLite::viridis(length(pop_sizes), alpha = 0.4)
for(i in 1:length(pop_sizes)){
polygon(c(t_vals, rev(t_vals)), c(dO1_vals[, i], rep(0, length(t_vals))), col = col_O1[i], border = NA)
}

legend("topright",
legend = c("Collection times", paste0("N=", pop_sizes)),
fill = c(col_T, col_O1),
border = NA,
bg = "white")

