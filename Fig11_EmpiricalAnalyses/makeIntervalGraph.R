library(ggplot2)
library(dplyr)
library(tidyr)

plot_intervals_by_type  =  function(df, covariateName,xlim) {
  library(ggplot2)
  library(dplyr)

  # Map original types to new descriptive labels
  df  =  df %>%
    mutate(type = case_when(
      type == "onset"    ~ "Onset (O)",
      type == "observed" ~ "Observed (T)",
      type == "duration" ~ "Duration (D)",
      TRUE ~ as.character(type)
    ))

  sample_breaks <- scales::breaks_log(n = 5)(df$sample_size)

  # Order species by mean observed value (using original observed type)
  order_species  =  df %>%
    filter(type == "Observed (T)") %>%
    arrange(mean) %>%
    pull(taxon.name)

  df$taxon.name  =  factor(df$taxon.name, levels = order_species)
  df$type  =  factor(df$type, levels = c("Observed (T)", "Onset (O)", "Duration (D)"))

  # Calculate overall means per type for vertical lines
  means_by_type  =  df %>%
    group_by(type) %>%
    summarize(overall_mean = mean(mean, na.rm = TRUE))

#x_min  =  min(df$lower, na.rm = TRUE)
#x_range  =  diff(range(df$lower, df$upper, na.rm = TRUE))
#padding  =  0.05 * x_range  # 5% padding

  p = ggplot(df) +
    geom_segment(aes(x = lower, xend = upper, y = taxon.name, yend = taxon.name, color = type), size = 0.6) +

    geom_point(aes(x = mean, y = taxon.name, color = type, fill = sample_size),
	  shape  = 21,        # circle that supports fill + color
  #fill   = "white",   # opaque center
  #alpha  = 1,         # fully opaque
  #color  = "black",   # border color
  stroke = 0.6,       # border thickness
  size   = 2.5
			   ) +
	scale_fill_gradientn(
  name    = "Sample size",
  #low     = "yellow",    # small samples → yellow
  #high    = "white",  # big samples  → white
  colours = c("yellow", "orange", "red", "purple", "blue"),
  values = c(0, 0.2, 0.4, 0.8, 1),
  na.value= "grey90",  # if any missing
  trans = "log10",
      #breaks = scales::trans_breaks("log10", function(x) 10^x),
    #labels = scales::trans_format("log10", scales::label_number(accuracy = 1))
  breaks = sample_breaks,
  labels = sample_breaks
) +

    geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +

    geom_text(aes(x = -Inf, y = taxon.name, label = taxon.name),
              hjust = 0, vjust = 0.5, nudge_x = 0.01,
              fontface = "italic", size = 3,
              inherit.aes = FALSE) +

	#geom_text(aes(x = -Inf, y = taxon.name, label = taxon.name),
          #hjust = 1, vjust = 0.5,
          #nudge_x = -0.02 * diff(range(df$lower, df$upper)),
          #fontface = "italic", size = 3,
          #inherit.aes = FALSE) + 

	#geom_text(aes(x = x_min - padding, y = taxon.name, label = taxon.name),
            #hjust = 1, vjust = 0.5,
            #fontface = "italic", size = 3,
            #inherit.aes = FALSE) +

  #scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(x_min - 2 * padding, NA)) +


    facet_wrap(~ type, ncol = 1, scales = "free_y") +

    scale_color_manual(values = c(
      "Observed (T)" = "purple",
      "Onset (O)" = "red",
      "Duration (D)" = "gray40"
    )) +

    geom_vline(data = means_by_type, aes(xintercept = overall_mean, color = type),
               linetype = "dotted", size = 0.8, show.legend = FALSE) +

    theme_minimal() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.margin = margin(5, 20, 5, 20)
    ) +
    xlab(paste0("Coefficient for ", covariateName))+
	xlim(xlim[1], xlim[2])

return(p)
}



plot_interval_graph  =  function(df, x_label = "Time") {
  # Check if mean columns exist
  has_means  =  all(c("onset_mean", "duration_mean", "observed_mean") %in% colnames(df))

  # Reshape data to long format for lower and upper
  df_long  =  df %>%
    pivot_longer(
      cols = ends_with(c("lower", "upper")),
      names_to = c("type", ".value"),
      names_pattern = "(.*)_(lower|upper)"
    ) %>%
    mutate(
      y = factor(type, levels = c("onset", "duration", "observed")),
      color = recode(type, onset = "red", duration = "gray40", observed = "purple")
    )

  # Add mean column: either from explicit means or midpoint
  if (has_means) {
    # Pivot longer the mean columns
    df_means  =  df %>%
      select(label, onset_mean, duration_mean, observed_mean) %>%
      pivot_longer(
        cols = c(onset_mean, duration_mean, observed_mean),
        names_to = "type",
        values_to = "mean"
      ) %>%
      mutate(type = sub("_mean$", "", type))

    # Join mean values into df_long by label and type
    df_long  =  df_long %>%
      left_join(df_means, by = c("label", "type"))
  } else {
    # Calculate midpoint mean
    df_long  =  df_long %>%
      mutate(mean = (lower + upper) / 2)
  }

  # Background rectangles per sample
  df_rect  =  df_long %>%
    distinct(label) %>%
    mutate(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 3.5)

  # Global mean of means for vertical lines
  mean_means  =  df_long %>%
    group_by(type) %>%
    summarise(global_mean = mean(mean, na.rm = TRUE), .groups = "drop") %>%
    mutate(color = recode(type, onset = "red", duration = "gray40", observed = "purple"))

  # Plot
 p =   ggplot() +
    geom_rect(
      data = df_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA,
      color = "black",
      linewidth = 0.3,
      alpha = 0.4
    ) +
    geom_segment(
      data = df_long,
      aes(x = lower, xend = upper, y = y, yend = y, color = color),
      size = 1
    ) +
    geom_point(
      data = df_long,
      aes(x = mean, y = y),
      shape = 21,
      fill = "white",
      size = 2.5,
      stroke = 0.7
    ) +
    geom_vline(
      data = mean_means,
      aes(xintercept = global_mean, color = color),
      linetype = "dotted",
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    facet_wrap(~ label, ncol = 1, strip.position = "left") +
    scale_color_identity() +
    scale_y_discrete(name = NULL) +
    scale_x_continuous(name = x_label, expand = expansion(mult = c(0.05, 0.05))) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0.4, "lines"),
      plot.margin = margin(4, 10, 4, 4),
      aspect.ratio = 0.15
    )
	return(p)
}


extractVector  =  function(resultsFile, metadataFile, species, covariateName) {
  data  =  read.table(resultsFile, header = TRUE, sep = "\t")
  meta = read.table(metadataFile, header=TRUE, sep='\t')

  Ns <- tibble(Species = species) %>%
  right_join(
    meta %>% filter(Variable == "N Filtered Specimens"),
    by = "Species"
  ) %>%
  pull(N)

  # Filter data once for relevant species and covariate
  filtered  =  data %>%
    filter(taxon.name %in% species, covariate == covariateName)

  # Helper function: extract vectors for a given type, preserving species order
  extract_for_type  =  function(type_name) {
    filtered %>%
      filter(type == type_name) %>%
      arrange(match(taxon.name, species)) %>%
      select(mean, q2.5, q97.5)
  }

  onset  =  extract_for_type("onset (O)")
  duration  =  extract_for_type("duration (D)")
  observed  =  extract_for_type("observed times (T)")

  # Return as list with named vectors, matching input species order
  list(
    meansO = onset$mean,
    q025O = onset$q2.5,
    q975O = onset$q97.5,
    meansD = duration$mean,
    q025D = duration$q2.5,
    q975D = duration$q97.5,
    meansT = observed$mean,
    q025T = observed$q2.5,
    q975T = observed$q97.5,
	N = Ns
  )
}


species = c("Anemone quinquefolia", "Camassia scilloides", "Cardamine concatenata", "Claytonia virginica", "Collinsia verna", "Dicentra cucullaria", "Enemion biternatum", "Erythronium americanum", "Mertensia virginica", "Podophyllum peltatum", "Primula meadia", "Sanguinaria canadensis", "Thalictrum thalictroides")
#species = c("Anemone quinquefolia", "Camassia scilloides", "Cardamine concatenata", "Claytonia virginica", "Collinsia verna", "Dicentra cucullaria", "Erythronium americanum", "Mertensia virginica", "Podophyllum peltatum", "Primula meadia", "Sanguinaria canadensis", "Thalictrum thalictroides")


covariate = "SpringMonthlyAverageTemp"
#data = extractVector(resultsFile="results.noOutliers.edit.txt", species=species, covariateName = covariate) 
#data = extractVector(resultsFile="TableX.empiricalResultsSummary.txt.csv", species=species, covariateName = covariate) 


	#print(data)
#df  =  tibble::tibble(
  #label = species,
  #onset_lower    = data$q025O,
  #onset_upper    = data$q975O,
  #onset_mean     = data$meansO,
  #duration_lower = data$q025D,
  #duration_upper = data$q975D,
  #duration_mean  = data$meansD,
  #observed_lower = data$q025T,
  #observed_upper = data$q975T,
  #observed_mean  = data$meansT
#)

#p = plot_interval_graph(df)
#print(p)


#df  =  tibble::tibble(
  #taxon.name = rep(species, 3),
  #type = rep(c("onset", "observed", "duration"), each = length(species)),
  #lower = c(data$q025T,data$q025O,data$q025D),
  #upper = c(data$q975T, data$q975O, data$q975D),
  #mean  = c(data$meansT,data$meansO,data$meansD),
#)

#p = plot_intervals_by_type(df, covariate)
#print(p)

covariates = c("SpringMonthlyAverageTemp", "AnnualMonthlyAverageTemp")
#covariates = c("SpringMonthlyAverageTemp", "Latitude", "Elevation", "Year","AnnualMonthlyAverageTemp","FirstQuarterMonthlyAverageTemp")

cnt = 1
for(covariate in covariates) {
#data = extractVector(resultsFile="results.noOutliers.edit.txt", species=species, covariateName = covariate) 
data = extractVector(resultsFile="TableX.empiricalResultsSummary.txt.csv", metadataFile = "TableX.dataSummary.txt.csv", species=species, covariateName = covariate) 
#data = extractVector(resultsFile="results.Test.txt", metadataFile = "TableX.dataSummary.txt.csv", species=species, covariateName = covariate) 
#data = extractVector(resultsFile="results.noOutliers.edit.txt", metadataFile = "TableX.dataSummary.txt.csv", species=species, covariateName = covariate) 
print("OK")
print(data)
#plot(data$meansT, data$meansO, main=covariate)
print(covariate)
print("correlation between observed vs. onset values")
print(summary(lm(data$meansT ~ data$meansO)))

lengthO = data$q975O-data$q025O
lengthD = data$q975D-data$q025D
lengthT = data$q975T-data$q025T
N = data$N

print("Correlation between CI length and sample size: onset")
print(summary(lm(lengthO ~ N)))
print("Correlation between CI length and sample size: duration")
print(summary(lm(lengthD ~ N)))
print("Correlation between CI length and sample size: observed")
print(summary(lm(lengthT ~ N)))


df  =  tibble::tibble(
  taxon.name = rep(species, 3),
  type = rep(c("onset", "observed", "duration"), each = length(species)),
  lower = c(data$q025O,data$q025T,data$q025D),
  upper = c(data$q975O, data$q975T, data$q975D),
  mean  = c(data$meansO,data$meansT,data$meansD),
  #sample_size = rep(log(data$N), 3)
  sample_size = rep(data$N, 3)
)

xlim=c(-1,1)
if(covariate=="SpringMonthlyAverageTemp") { 
	xlim = c(-12,7)
 p = plot_intervals_by_type(df, covariate, xlim)
}
if(covariate=="AnnualMonthlyAverageTemp") {
	xlim = c(-15,8)
 p = plot_intervals_by_type(df, covariate, xlim)
}
 ggsave(paste0("Fig14.EmpiricalSummary.", covariate, ".pdf"))
}


