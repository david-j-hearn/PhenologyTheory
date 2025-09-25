library(viridis)

set.seed(42)

data = read.table("results.firstPass.txt", header=T, sep='\t')
#data = read.table("results.X.AE.txt", header=T, sep='\t')
#data = read.table("temp.txt", header=T, sep='\t')
#data = read.table("output.Test.txt", header=T, sep='\t')
data = data[data$Number_Divergences==0,]
data = data[1:5000, ]
#data = data[1:1000, ]

#anchor error
anchorQ = data$Est_QR_Intercept_meanQ10 + data$Est_QR_Slope_meanQ10 * (1850 + 2025) / 2
#anchorQ = data$Est_QR_Intercept_meanQ10 + data$Est_QR_Slope_meanQ10 * (-2 + 24) / 2

anchorT = data$True_Anchor_meanOnset
anchorS = data$Est_Stan_Anchor_meanOnset
errS = abs(anchorS - anchorT)
errQ = abs(anchorQ - anchorT)
errs = errS < errQ
print("Proportion of time Stan error is less than quantile error for T")
print(mean(errs))

print("T test comparing QR and Stan error on estimate of mean onset")
print(t.test(errQ, errS))
print("mean Stan error on onset anchor")
print(mean(errS))
print("mean QR error on onset anchor")
print(mean(errQ))

xlim = range(errS, errQ)
nbreaks = 75
b = seq(xlim[1], xlim[2], length.out = nbreaks)
hist(errS, xlim=xlim, breaks=b, col=rgb(1,0,0,0.5), border=NA)
hist(errQ, xlim=xlim, breaks=b, col=rgb(0,0,1,0.5), border=NA, add=T)

mlms = lm(errS ~ data$True_Anchor_meanDuration)
print("Stan anchor onset error as a function of mean duration")
print(summary(mlms))		#error is not correlated with duration

print("QR anchor onset error as a function of mean duration")
mlmq = lm(errQ ~ data$True_Anchor_meanDuration)
print(summary(mlmq))		#error is correlated with duration

dev.new()
plot(data$True_Anchor_meanDuration, errQ, pch=16, col=rgb(0,0,1,0.1))
points(data$True_Anchor_meanDuration, errS, pch=16, col=rgb(1,0,0,0.1))

dev.new()
plot(data$True_Sigma, errQ, pch=16, col=rgb(0,0,1,0.1))
points(data$True_Sigma, errS, pch=16, col=rgb(1,0,0,0.1))

print("Stan anchor onset error as a function of  sigma")
mlms.sig = lm(errS ~ data$True_Sigma)
print(summary(mlms.sig))		#error is positively correlated with sigma

print("QR anchor onset error as a function of  sigma")
mlmq.sig = lm(errQ ~ data$True_Sigma)
print(summary(mlmq.sig))		#error is negatively correlated with sigma

print("Stan onset error as a function of  sigma and duration")
mlms.sig.dur = lm(errS ~ data$True_Sigma + data$True_Anchor_meanDuration)
print(summary(mlms.sig.dur))		#error is positively correlated with sigma

print("QR onset error as a function of  sigma and duration")
mlmq.sig.dur = lm(errQ ~ data$True_Sigma + data$True_Anchor_meanDuration)
print(summary(mlmq.sig.dur))		#error is negatively correlated with sigma

#Slope error
slopeQ = data$Est_QR_Slope_meanQ10
slopeT = data$True_Slope_meanOnset
slopeS = data$Est_Stan_Slope_meanOnset
errS.s = abs(slopeS - slopeT)
errQ.s = abs(slopeQ - slopeT)
errs.s = errS < errQ
mean(errs.s)
xlim = range(errS.s, errQ.s)
nbreaks = 75
b = seq(xlim[1], xlim[2], length.out = nbreaks)

dev.new()
hist(errS.s, xlim=xlim, breaks=b, col=rgb(1,0,0,0.5), border=NA)
hist(errQ.s, xlim=xlim, breaks=b, col=rgb(0,0,1,0.5), border=NA, add=T)

print("Stan onset slope error as a function of duration")
mlms = lm(errS.s ~ data$True_Anchor_meanDuration)
print(summary(mlms))		#error is not correlated with duration

print("QR onset slope error as a function of duration")
mlmq = lm(errQ.s ~ data$True_Anchor_meanDuration)
print(summary(mlmq))		#error is correlated with duration

dev.new()
plot(data$True_Anchor_meanDuration, errQ.s, pch=16, col=rgb(0,0,1,0.1))
points(data$True_Anchor_meanDuration, errS.s, pch=16, col=rgb(1,0,0,0.1))

dev.new()
plot(data$True_Sigma, errQ.s, pch=16, col=rgb(0,0,1,0.1))
points(data$True_Sigma, errS.s, pch=16, col=rgb(1,0,0,0.1))

print("Stan onset slope error as a function of sigma")
mlms.sig = lm(errS.s ~ data$True_Sigma)
print(summary(mlms.sig))		#error is positively correlated with sigma

print("QR onset slope error as a function of sigma")
mlmq.sig = lm(errQ.s ~ data$True_Sigma)
print(summary(mlmq.sig))		#error is negatively correlated with sigma

print("Stan onset slope error as a function of sigma and duration anchor")
mlms.sig.dur = lm(errS.s ~ data$True_Sigma + data$True_Anchor_meanDuration)
print(summary(mlms.sig.dur))		#error is positively correlated with sigma

print("QR onset slope error as a function of sigma and duration anchor")
mlmq.sig.dur = lm(errQ.s ~ data$True_Sigma + data$True_Anchor_meanDuration)
print(summary(mlmq.sig.dur))		#error is negatively correlated with sigma

#split violin plot color coded by within-bar mean duration to highlight correlation for q10
#USE THIS ONE:
#__________________________________________
dev.new()
value <- c(errS, errQ)
other <- c(data$True_Anchor_meanDuration, data$True_Anchor_meanDuration)
#other <- c(data$True_Sigma, data$True_Sigma)
category = rep(c("HMC GP", "Q10%"), each = length(errS))

num_bins <- 30
breaks <- hist(value, breaks = num_bins, plot = FALSE)$breaks

# Create red-to-blue color palette function
#red_blue_pal <- colorRampPalette(c("red", "blue", "blue"))
#red_blue_pal <- colorRampPalette(c("red", "red", "blue", "blue", "blue" ))
#red_blue_pal <- colorRampPalette(c("darkred", "red", "orange", "yellow"))
#red_blue_pal <- colorRampPalette(c("darkgreen", "lightgreen", "lightblue", "blue"))
red_blue_pal <- function(n) viridis::magma(n)

categories <- unique(category)

# Calculate histograms and mean 'other' per bin separately for each category
hist_list <- list()
mean_other_by_bin_list <- list()
bin_colors_list <- list()

for (cat in categories) {
  vals_cat <- value[category == cat]
  other_cat <- other[category == cat]
  
  # Histogram for category
  h <- hist(vals_cat, breaks = breaks, plot = FALSE)
  hist_list[[cat]] <- h
  
  # Mean 'other' per bin for this category
  bin_ids_cat <- cut(vals_cat, breaks = breaks, include.lowest = TRUE)
  mean_other_cat <- tapply(other_cat, bin_ids_cat, mean)
  mean_other_by_bin_list[[cat]] <- mean_other_cat
  
  # Map mean_other to colors
  bin_colors_list[[cat]] <- red_blue_pal(length(mean_other_cat))[rank(mean_other_cat)]
}

# Max count for scaling violin width
max_count <- max(sapply(hist_list, function(h) max(h$counts)))

x_center <- 1
x_width <- 0.4

pdf("Fig10.quantileRegressionComparison.pdf")
# Empty plot with no x-axis ticks or labels
plot(NA, NA,
     xlim = c(x_center - x_width - 0.1, x_center + x_width + 0.1),
     ylim = range(breaks),
     xlab = "", ylab = "Absolute Error Mean Onset (Days)",
     xaxt = "n",
     #main = "Split Violin Histogram with Category-specific Coloring")
     main = NULL)

# Add category labels below
text(x = c(x_center - x_width/2, x_center + x_width/2),
     y = par("usr")[3] - 0.1 * diff(par("usr")[3:4]),
     labels = categories,
     xpd = TRUE, cex = 1.2)

# Draw left side (category A)
cat_left <- categories[1]
h_left <- hist_list[[cat_left]]
colors_left <- bin_colors_list[[cat_left]]

for (i in seq_along(h_left$counts)) {
  ybottom <- h_left$breaks[i]
  ytop <- h_left$breaks[i + 1]
  count <- h_left$counts[i]
  
  col <- adjustcolor(colors_left[i], alpha.f = 0.7)
  
  rect(
    xleft = x_center - (count / max_count) * x_width,
    xright = x_center,
    ybottom = ybottom,
    ytop = ytop,
    col = col,
    border = NA
  )
}

# Draw right side (category B)
cat_right <- categories[2]
h_right <- hist_list[[cat_right]]
colors_right <- bin_colors_list[[cat_right]]

for (i in seq_along(h_right$counts)) {
  ybottom <- h_right$breaks[i]
  ytop <- h_right$breaks[i + 1]
  count <- h_right$counts[i]
  
  col <- adjustcolor(colors_right[i], alpha.f = 0.7)
  
  rect(
    xleft = x_center,
    xright = x_center + (count / max_count) * x_width,
    ybottom = ybottom,
    ytop = ytop,
    col = col,
    border = NA
  )
}

abline(v = x_center, col = "black", lwd = 0.5)


legend_width <- 0.3
legend_height <- 1
legend_y <- 15

legend_x_start <- c(x_center - legend_width - 0.1, x_center + 0.1)

draw_smooth_legend <- function(x_start, y, min_val, max_val, palette_func, label) {
  n <- 100
  colors <- palette_func(n)
  xs <- seq(x_start, x_start + legend_width, length.out = n + 1)
  
  for (i in seq_len(n)) {
    rect(xs[i], y, xs[i+1], y + legend_height, col = colors[i], border = NA)
  }
  rect(x_start, y, x_start + legend_width, y + legend_height, border = "black")
  
  text(x_start, y - 0.8 * legend_height, labels = round(min_val, 2),
       adj = 0, xpd = TRUE, cex = 0.8)
  text(x_start + legend_width, y - 0.8 * legend_height, labels = round(max_val, 2),
       adj = 1, xpd = TRUE, cex = 0.8)
  text(x_start + legend_width / 2, y + legend_height + 0.8 * legend_height, labels = label,
       xpd = TRUE, cex = 1, font = 2)
}

# Get min/max values for each category's means (ignore NAs)
min_left <- min(mean_other_by_bin_list[[cat_left]], na.rm = TRUE)
max_left <- max(mean_other_by_bin_list[[cat_left]], na.rm = TRUE)
min_right <- min(mean_other_by_bin_list[[cat_right]], na.rm = TRUE)
max_right <- max(mean_other_by_bin_list[[cat_right]], na.rm = TRUE)

draw_smooth_legend(legend_x_start[1], legend_y, min_left, max_left, red_blue_pal, "Mean Duration")
draw_smooth_legend(legend_x_start[2], legend_y, min_right, max_right, red_blue_pal, "Mean Duration")

dev.off()
#draw_smooth_legend(legend_x_start[1], legend_y, min_left, max_left, red_blue_pal, paste("Mean 'other' in", cat_left))
#draw_smooth_legend(legend_x_start[2], legend_y, min_right, max_right, red_blue_pal, paste("Mean 'other' in", cat_right))

#draw_vertical_legend <- function(x_pos, y_bottom, y_top, min_val, max_val, palette_func, label) {
  #n <- 100
  #ys <- seq(y_bottom, y_top, length.out = n + 1)
  #colors <- palette_func(n)
  #width <- 0.03
  #
  #for (i in seq_len(n)) {
    #rect(x_pos - width/2, ys[i], x_pos + width/2, ys[i+1], col = colors[i], border = NA)
  #}
  #rect(x_pos - width/2, y_bottom, x_pos + width/2, y_top, border = "black")
  #
  ## Value labels
  #text(x_pos + width, y_bottom, labels = round(min_val, 2), adj = c(0, 1), cex = 0.7, xpd = TRUE)
  #text(x_pos + width, y_top, labels = round(max_val, 2), adj = c(0, 0), cex = 0.7, xpd = TRUE)
  #
  ## Legend title
  #text(x_pos, y_top + 0.5, label, cex = 0.9, font = 2, xpd = TRUE)
#}
#
## Y-position range for legends
#y_bottom <- min(breaks)
#y_top <- max(breaks)
#
## Left legend (Category A)
#min_left <- min(mean_other_by_bin_list[[cat_left]], na.rm = TRUE)
#max_left <- max(mean_other_by_bin_list[[cat_left]], na.rm = TRUE)
#draw_vertical_legend(x_pos = x_center - x_width - 0.15, y_bottom, y_top,
                     #min_val = min_left, max_val = max_left,
                     #palette_func = red_blue_pal,
                     #label = paste("Mean 'other'\n(", cat_left, ")"))
#
## Right legend (Category B)
#min_right <- min(mean_other_by_bin_list[[cat_right]], na.rm = TRUE)
#max_right <- max(mean_other_by_bin_list[[cat_right]], na.rm = TRUE)
#draw_vertical_legend(x_pos = x_center + x_width + 0.15, y_bottom, y_top,
                     #min_val = min_right, max_val = max_right,
                     #palette_func = red_blue_pal,
                     #label = paste("Mean 'other'\n(", cat_right, ")"))

#__________________________________________

dev.new()

df = data.frame(value = c(errS, errQ),
	group = rep(c("errS", "errQ"), each = length(errS))
	)
library(ggplot2)

ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_violin(trim = FALSE) +
  theme_minimal() +
  labs(title = "Violin Plot of Two Data Series",
       x = "Group",
       y = "Value")


#__________________________________________
dev.new()

value <- c(errS, errQ)
other <- c(data$True_Anchor_meanDuration, data$True_Anchor_meanDuration)
#category <- rep(c("A", "B"), 1000, replace = TRUE)
category = rep(c("errS", "errQ"), each = length(errS))

num_bins <- 30
breaks <- hist(value, breaks = num_bins, plot = FALSE)$breaks

# Mean of 'other' per bin (all data)
bin_ids_all <- cut(value, breaks = breaks, include.lowest = TRUE)
mean_other_by_bin <- tapply(other, bin_ids_all, mean)

# Red-to-blue gradient palette
red_blue_pal <- colorRampPalette(c("red", "blue"))
bin_colors <- red_blue_pal(length(mean_other_by_bin))[rank(mean_other_by_bin)]

categories <- unique(category)

# Calculate histograms by category
hist_list <- lapply(categories, function(cat) {
  vals_cat <- value[category == cat]
  hist(vals_cat, breaks = breaks, plot = FALSE)
})

# Max count across both categories for scaling width
max_count <- max(sapply(hist_list, function(h) max(h$counts)))

# Center position for the split violin
x_center <- 1
x_width <- 0.4  # max half-width of violin sides

# Empty plot, no x-axis labels (we will add custom label)
plot(NA, NA,
     xlim = c(x_center - x_width - 0.1, x_center + x_width + 0.1),
     ylim = range(breaks),
     xlab = "", ylab = "Value",
     xaxt = "n", main = "Split Violin Histogram by Category")

# Add category labels below center
text(x = c(x_center - x_width/2, x_center + x_width/2), y = par("usr")[3] - 0.1*(diff(par("usr")[3:4])),
     labels = categories, xpd = TRUE, cex = 1.2)

# Draw left side bars for first category (e.g. "A")
h_left <- hist_list[[1]]
for (i in seq_along(h_left$counts)) {
  ybottom <- h_left$breaks[i]
  ytop <- h_left$breaks[i + 1]
  count <- h_left$counts[i]
  
  col <- adjustcolor(bin_colors[i], alpha.f = 0.7)
  
  rect(
    xleft = x_center - (count / max_count) * x_width,
    xright = x_center,
    ybottom = ybottom,
    ytop = ytop,
    col = col,
    border = NA
  )
}

# Draw right side bars for second category (e.g. "B")
h_right <- hist_list[[2]]
for (i in seq_along(h_right$counts)) {
  ybottom <- h_right$breaks[i]
  ytop <- h_right$breaks[i + 1]
  count <- h_right$counts[i]
  
  col <- adjustcolor(bin_colors[i], alpha.f = 0.7)
  
  rect(
    xleft = x_center,
    xright = x_center + (count / max_count) * x_width,
    ybottom = ybottom,
    ytop = ytop,
    col = col,
    border = NA
  )
}

#__________________________________________
dev.new()

value <- c(errS, errQ)
other <- c(data$True_Anchor_meanDuration, data$True_Anchor_meanDuration)
#category <- rep(c("A", "B"), 1000, replace = TRUE)
category = rep(c("errS", "errQ"), each = length(errS))

num_bins <- 30
breaks <- hist(value, breaks = num_bins, plot = FALSE)$breaks

# Mean of 'other' per bin (all data)
bin_ids_all <- cut(value, breaks = breaks, include.lowest = TRUE)
mean_other_by_bin <- tapply(other, bin_ids_all, mean)

# Red-to-blue gradient palette
red_blue_pal <- colorRampPalette(c("red", "blue"))
bin_colors <- red_blue_pal(length(mean_other_by_bin))[rank(mean_other_by_bin)]

categories <- unique(category)

# Calculate histograms by category
hist_list <- lapply(categories, function(cat) {
  vals_cat <- value[category == cat]
  hist(vals_cat, breaks = breaks, plot = FALSE)
})

# Max count for x limits of violins
max_count <- max(sapply(hist_list, function(h) max(h$counts)))

# X positions for categories
x_positions <- 1:length(categories)
x_width <- 0.4  # half-width for violin bars on x-axis

# Set up empty plot with space for categories on x-axis
plot(NA, NA,
     xlim = c(0.5, length(categories) + 0.5),
     ylim = range(breaks),
     xlab = "", ylab = "Value",
     xaxt = "n",  # suppress x axis ticks and labels
     main = "Mirrored Horizontal Violin Plot by Category")

# Add category names at x positions
axis(side = 1, at = x_positions, labels = categories, tick = FALSE, cex.axis = 1.2)

# Draw violins for each category
for (i in seq_along(categories)) {
  h <- hist_list[[i]]
  
  for (j in seq_along(h$counts)) {
    ybottom <- h$breaks[j]
    ytop <- h$breaks[j + 1]
    count <- h$counts[j]
    
    col <- adjustcolor(bin_colors[j], alpha.f = 0.7)
    
    # Left side (mirrored left of category position)
    rect(
      xleft = x_positions[i] - count / max_count * x_width,
      xright = x_positions[i],
      ybottom = ybottom,
      ytop = ytop,
      col = col,
      border = NA
    )
    
    # Right side (mirrored right of category position)
    rect(
      xleft = x_positions[i],
      xright = x_positions[i] + count / max_count * x_width,
      ybottom = ybottom,
      ytop = ytop,
      col = col,
      border = NA
    )
  }
}

