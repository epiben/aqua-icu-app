# Define server logic required to draw a histogram
server <- function(input, output, session) {
	# Values from cdata returned as text
	clientdataText <- reactive({
		cdata <- session$clientData
		cnames <- names(cdata)

		allvalues <- lapply(cnames, function(name) {
			paste(name, cdata[[name]], sep = " = ")
		})
		paste(allvalues, collapse = "\n")
	})

	# ===

	outcome <- reactive({
		sprintf("%s__%s", input$start_of_followup, input$operationalisation)
	})

	output$the_plot <- renderPlot({
		shape_var <- input$shape_var
		colour_var <- input$colour_var

		# shape_var <- "final_hrqol"
		# colour_var <- "n_patients_per_arm"

		strat_vars <- names(all_strat_vars)[!names(all_strat_vars) %in% c(shape_var, colour_var)]

		make_fct <- function(x) {
			level_idx <- names(beautify_labels) %in% unique(x)
			factor(x, names(beautify_labels[level_idx]), beautify_labels[level_idx])
		}

		df <- comparisons %>%
			filter(outcome == outcome() & analysis == input$analysis) %>%
			# filter(outcome == "primary__hrqol_at_eof" & analysis == "all") %>%
			mutate(across(names(all_strat_vars), make_fct)) %>%
			arrange(across(rev(strat_vars))) %>%
			mutate(
				x = seq_len(n()),
				colour_column = !!sym(colour_var),
				shape_column = if (colour_var != shape_var) !!sym(shape_var) else colour_column
			)

		n_shape_colour_levels <- n_distinct(select(df, shape_column, colour_column))
		bar_combos <- distinct(select(df, all_of(strat_vars))) %>%
			slice(rep(seq_len(nrow(.)), each = n_shape_colour_levels))

		bar_coords <- list()
		for (g in strat_vars) {
			vals <- bar_combos[[g]]
			grps <- vals != data.table::shift(vals)
			grps[is.na(grps)] <- TRUE # needed to gracefully vals of any type
			tmp <- lapply(
				setNames(split(seq_len(nrow(df)), cumsum(grps)), vals[grps]),
				function(.) tibble(label_x = mean(range(.)), xmin = min(.), xmax = max(.))
			)
			bar_coords[[g]] <- bind_rows(tmp, .id = "label")
		}

		bar_coords <- bind_rows(bar_coords, .id = "grouping_var") %>%
			mutate(
				across(# ensure correct order of vertical axis
					grouping_var,
					\(x) factor(x, levels = rev(count(., grouping_var, sort = TRUE)$grouping_var))
				),
				is_first_level = as.integer(grouping_var) == max(as.integer(grouping_var))
			) %>%
			group_by(grouping_var) %>%
			mutate(draw_ribbon = seq_len(n()) %% 2 == 1) %>%
			ungroup()
		labels <- all_strat_vars[levels(bar_coords$grouping_var)]
		levels(bar_coords$grouping_var) <- as.list(setNames(names(labels), labels))

		# Shaded ribbons to help see groups belonging to the lowest level of stratification
		ribbon_coords <- tibble(
			x = seq_len(nrow(df)),
			grp = rep(seq_len(nrow(df) / n_shape_colour_levels), each = n_shape_colour_levels),
			draw_ribbon = rep(seq_len(nrow(df) / n_shape_colour_levels), each = n_shape_colour_levels) %% 2 == 1
		) %>%
			filter(draw_ribbon == TRUE) %>%
			group_by(grp) %>%
			summarise(xmin = min(x) - 0.5, xmax = max(x) + 0.5)

		bar_coords %>%
			group_by(grouping_var) %>%
			summarise(xmin = min(xmin), xmax = max(xmax))

		bar_plot <- ggplot(bar_coords, aes(y = grouping_var)) +
			annotate("point", x = rep(range(df$x), 3), y = rep(unique(bar_coords$grouping_var), each = 2), colour = NA) +
				# these non-filled points ensure alignment between x axis in bar and point plots
			geom_tile(aes(x = label_x, width = xmax - xmin + 1), ~ filter(., draw_ribbon), alpha = 0.05) +
			annotate("tile", y = unique(bar_coords$grouping_var), x = 0, height = 1, width = Inf, fill = NA, colour = "white", linewidth = 1) +
			# geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, y = NULL), ribbon_coords, alpha = 0.05) +
			# geom_errorbarh(aes(xmin = xmin - 0.5, xmax = xmax + 0.5), ~ filter(., !is_first_level), linewidth = 0.2, height = 0.1) +
			# geom_label(aes(x = label_x, label = label), ~ filter(., !is_first_level), size = input$strat_label_size/.pt, hjust = 0.5) +
			geom_text(aes(x = label_x, label = label), size = input$strat_label_size/.pt, hjust = 0.5) +
			scale_x_reverse(expand = c(0.01, 0.01)) +
			scale_y_discrete(expand = c(0.01, 0.5)) +
			theme_minimal() +
			theme(
				panel.grid = element_blank(),
				axis.title = element_blank(),
				axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
				axis.text.y = element_blank()
			) +
			coord_flip()

		helper_line <- if (input$y_var == "rejection_proportion") {
			geom_hline(yintercept = 0.05, linewidth = 0.25, linetype = 2)
			} else {
				geom_blank()
			}

		point_plot <- ggplot(df) +
			helper_line +
			geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), ribbon_coords, alpha = 0.05) +
			geom_point(aes(x = x, y = get(input$y_var), shape = if (shape_var != colour_var) shape_column else NULL, colour = colour_column)) +
			scale_colour_brewer(palette = input$brewer_style) +
			scale_y_continuous(
				expand = c(0.01, 0.01),
				limits = if (input$y_var == "rejection_proportion") c(0, 1) else c(NA, NA),
				sec.axis = sec_axis(
					trans = ~ .,
					labels = if (input$y_var == "rejection_proportion") scales::percent else waiver(),
					name = names(performance_metrics_choices)[performance_metrics_choices == input$y_var]
				)
			) +
			scale_x_reverse(expand = c(0.01, 0.01)) +
			theme_minimal() +
			theme(
				axis.text.x.top = element_text(size = 11),
				axis.text.x.bottom = element_blank(),
				axis.text.y = element_blank(),
				axis.title.y = element_blank(),
				axis.title.x.top = element_text(size = 11, vjust = 1.5, face = "bold"),
				axis.title.x.bottom = element_blank(),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
				legend.position = "right",
				legend.title = element_text(size = 11),
				legend.text = element_text(size = 11),
				legend.spacing = grid::unit(2, "cm")
			) +
			guides(
				colour = guide_legend(title = paste0(all_strat_vars[colour_var], ":")),
				shape = guide_legend(title = paste0(all_strat_vars[shape_var], ":"))
			) +
			coord_flip()

		# Vertical layout
		bar_plot + point_plot +
			plot_layout(nrow = 1, widths = c(1, input$plot_bars_ratio))
	})

	output$main_panel <- renderUI({
		plotOutput("the_plot", height = sprintf("%ivh", round(input$relative_plot_height)))
		# box(width = 12,
		# 		,
		# 	collapsed = FALSE,
		# 	collapsible = FALSE
		# )
	})

}

server
