# Define server logic required to draw a histogram
server <- function(input, output) {

	output$the_plot <- renderPlot({
		shape_var <- input$shape_var
		colour_var <- input$colour_var

		strat_vars <- all_strat_vars[!all_strat_vars %in% c(shape_var, colour_var)]

		data.table::setorderv(comparisons, all_strat_vars)
		plot_dt <- comparisons[outcome == "primary__hrqol_at_eof" & analysis == "all"]
		plot_dt[
			,
			`:=`(
				x = seq_len(.N),
				colour_column = factor(get(colour_var), ordered = TRUE),
				shape_column = factor(get(shape_var)),
				line_group = do.call(paste, as.list(plot_dt[, mget(c(strat_vars, shape_var))]))
			)
		]

		n_shape_colour_levels <- nrow(unique(plot_dt[, mget(c(shape_var, colour_var))]))
		bar_combos <- unique(plot_dt[, ..strat_vars])[rep(seq_len(.N), each = n_shape_colour_levels), ]
		bar_coords <- list()
		for (g in strat_vars) {
			vals <- bar_combos[[g]]
			grps <- vals != data.table::shift(vals, fill = 0)
			tmp <- lapply(
				setNames(split(seq_len(nrow(plot_dt)), cumsum(grps)), vals[grps]),
				function(.) data.table::data.table(label_x = mean(range(.)), xmin = min(.), xmax = max(.))
			)
			bar_coords[[g]] <- data.table::rbindlist(tmp, idcol = "label")
		}
		bar_coords <- data.table::rbindlist(bar_coords, idcol = "grouping_var")

		# Ensure vertical axis is in the right order
		bar_coords[, grouping_var := ifelse(grouping_var == "mortality", "mortality reduction", grouping_var)]
		bar_coords[, grouping_var := factor(
			grouping_var,
			levels = bar_coords[, .N, by = grouping_var][order(N)][["grouping_var"]]
		)]

		bar_coords[, `:=`(
			grouping_var = str_replace_all(as.character(grouping_var), beautify_labels),
			label = str_replace_all(label, beautify_labels)
		)]

		bar_plot <- ggplot(bar_coords, aes(y = grouping_var)) +
			geom_errorbarh(aes(xmin = xmin - 0.5, xmax = xmax + 0.5), linewidth = 0.25, height = 0.2) +
			geom_text(aes(x = label_x, label = label), size = 10/.pt, vjust = 1.5) +
			scale_x_continuous(expand = c(0.01, 0.01)) +
			theme_minimal() +
			theme(
				panel.grid = element_blank(),
				axis.title = element_blank(),
				axis.text.y = element_text(size = 10),
				axis.text.x = element_blank()
			)

		# Shaded ribbons to help see groups belonging to the lowest level of stratification
		ribbon_coords <- data.table(
			x = seq_len(nrow(plot_dt)),
			grp = rep(seq_len(nrow(plot_dt) / n_shape_colour_levels), each = n_shape_colour_levels),
			draw_ribbon = rep(seq_len(nrow(plot_dt) / n_shape_colour_levels), each = n_shape_colour_levels) %% 2 == 1
		) %>%
			.[draw_ribbon == TRUE] %>%
			.[, .(xmin = min(x) - 0.5, xmax = max(x) + 0.5), by = "grp"]

		point_plot <- ggplot(plot_dt) +
			# geom_line(aes(x = x, y = get(input$y_var), group = line_group), linewidth = 0.25, colour = "grey90") +
			# geom_hline(yintercept = 0.05, linewidth = 0.25, linetype = 2) +
			geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), ribbon_coords, alpha = 0.05) +
			geom_point(aes(x = x, y = get(input$y_var), shape = shape_column, colour = colour_column)) +
			scale_colour_brewer(palette = input$brewer_style) +
			# scale_y_continuous(limits = c(0, 1), labels = scales::percent, expand = c(0.01, 0.01)) +
			scale_y_continuous(expand = c(0.01, 0.01)) +
			scale_x_continuous(expand = c(0.01, 0.01)) +
			theme_minimal() +
			theme(
				axis.text.y = element_text(size = 10),
				axis.text.x = element_blank(),
				axis.title.x = element_blank(),
				panel.grid.major.x = element_blank(),
				panel.grid.minor.x = element_blank(),
				legend.position = "top",
				legend.title = element_text(size = 10),
				legend.text = element_text(size = 10)
			) +
			guides(
				colour = guide_legend(title = colour_var),
				shape = guide_legend(title = shape_var)
			)

		# Vertical layout
		point_plot / bar_plot +
			plot_layout(heights = c(4, 1))
	})

	output$distPlot <- renderPlot({
		# generate bins based on input$bins from ui.R
		x    <- faithful[, 2]
		bins <- seq(min(x), max(x), length.out = input$bins + 1)

		# draw the histogram with the specified number of bins
		hist(x, breaks = bins, col = 'darkgray', border = 'white',
				 xlab = 'Waiting time to next eruption (in mins)',
				 main = 'Histogram of waiting times')
	})
}

server
