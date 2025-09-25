plot_heatmap_from_df <- function(df,
								 row_var = "Row",
								 col_var = "Col",
								 value_var = "Value",
								 row_labels = NULL,
								 col_labels = NULL,
								 row_axis_label = "Row",
								 col_axis_label = "Column",
								 low_color = "white",
								 high_color = "steelblue",
								 midpoint = NULL,
								 show_values = TRUE,
								 value_digits = 1,
								 show_legend = TRUE,
								 limits = NULL,
								 highlight_rows = NULL,
								 title = NULL) {
	library(ggplot2)
	library(dplyr)

	if(is.null(limits)) { limits = c(0,1) }
	if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
	library(ggplot2)

	if (!all(c(row_var, col_var, value_var) %in% colnames(df))) {
		stop("Input data frame must contain the specified row, column, and value variables.")
	}

	plot_df <- df[, c(row_var, col_var, value_var)]
	colnames(plot_df) <- c("Row", "Col", "Value")

	# Convert to factors with labels if provided
	if (!is.null(row_labels)) {
		plot_df$Row <- factor(plot_df$Row, levels = sort(unique(plot_df$Row)), labels = row_labels)
	} else {
		plot_df$Row <- factor(plot_df$Row, levels = sort(unique(plot_df$Row)))
	}

	if (!is.null(col_labels)) {
		plot_df$Col <- factor(plot_df$Col, levels = sort(unique(plot_df$Col)), labels = col_labels)
	} else {
		plot_df$Col <- factor(plot_df$Col, levels = sort(unique(plot_df$Col)))
	}

	# Create display labels for text
	plot_df$Label <- if (show_values) round(plot_df$Value, value_digits) else ""

	# Fill scale
	fill_scale <- if (!is.null(midpoint)) {
		scale_fill_gradient2(low = low_color, high = high_color, midpoint = midpoint, limits = limits)
	} else {
		scale_fill_gradient(low = low_color, high = high_color, limits = limits)
	}

	# Core plot
	p <- ggplot(plot_df, aes(x = Col, y = Row, fill = Value)) +
		geom_tile(color = "white") +
		fill_scale +
		coord_fixed() +
		scale_y_discrete(limits = rev(levels(plot_df$Row))) +
		labs(x = col_axis_label,
			 y = row_axis_label,
			 fill = "Value",
			 title = title) +
		theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
	#theme_minimal()

	# Add tile labels
	if (show_values) {
		p <- p + geom_text(aes(label = Label), size = 3)
	}

	# Hide legend if requested
	if (!show_legend) {
		p <- p + theme(legend.position = "none")
	}

	if(!is.null(highlight_rows)) {
		highlight_rects <- plot_df %>%
			filter(Row %in% highlight_rows) %>%
			distinct(Row) %>%
			mutate(
				   #ymin = as.numeric(factor(Row, levels = rev(LETTERS[1:10]))) - 0.5,
				   #ymax = ymin + 1
				               yval = as.numeric(factor(Row, levels = rev(levels(plot_df$Row)))),
            ymin = yval - 0.5,
            ymax = yval + 0.5,
            xmin = 0.5,
            xmax = length(unique(plot_df$Col)) + 0.5
			)

			p = p + 
				geom_rect(data = highlight_rects,
						  #aes(xmin = 0.5, xmax = 10.5, ymin = ymin, ymax = ymax),
						  #fill = NA, color = "red", size = 1)
						  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE,
                  fill = NA, color = "red", linewidth = 1)
	}

	return(p)
}

