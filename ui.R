source("helpers.R")

# Define UI for application that draws a histogram
sidebar <- dashboardSidebar(
	tags$style(
		type = "text/css",
		"#black_labels {color: black}"
	),
	disable = FALSE,
	width = 300,
	br(),
	div(
		id = "black_labels",
		box(
			title = "Data",
			width = 12,
			collapsed = FALSE,
			collapsible = FALSE,
			solidHeader = FALSE,
			selectInput(
				"operationalisation",
				"Operationalisation",
				operationalisation_choices,
				multiple = FALSE,
				selected = "hrqol_at_eof"
			),
			selectInput(
				"start_of_followup",
				"Start of follow-up",
				start_of_followup_choices,
				multiple = FALSE,
				selected = "primary"
			),
			selectInput(
				"analysis",
				"Analaysis",
				analysis_choices,
				multiple = FALSE,
				selected = "all"
			),
			selectInput(
				"y_var",
				"Performance metric",
				performance_metrics_choices,
				multiple = FALSE,
				selected = "rejection_proportion"
			),
			selectInput(
				"shape_var",
				"Shape variable",
				shape_colour_choices,
				multiple = FALSE,
				selected = "final_hrqol"
			),
			selectInput(
				"colour_var",
				"Colouring variable",
				shape_colour_choices,
				multiple = FALSE,
				selected = "n_patients_per_arm"
			)
		),
		box(
			title = "Filtering",
			width = 12,
			collapsed = TRUE,
			collapsible = TRUE,
			solidHeader = TRUE,
			checkboxGroupInput(
				"filter_mortality",
				"Mortality reduction (%)",
				selected = c("0", "2.5", "5", "10"),
				choices = c("0", "2.5", "5", "10"),
				inline = TRUE
			),
			checkboxGroupInput(
				"filter_prop_mortality_benefitters",
				"Mortality benefitters (%)",
				selected = c("0", "5", "10", "15"),
				choices = c("0", "5", "10", "15"),
				inline = TRUE
			),
			checkboxGroupInput(
				"filter_acceleration_hrqol",
				"Acceleration in HRQoL (%)",
				selected = c("0", "2", "5", "10"),
				choices = c("0", "2", "5", "10"),
				inline = TRUE
			),
			checkboxGroupInput(
				"filter_final_hrqol",
				"Improved final HRQoL (%)",
				selected = c("0", "10", "20"),
				choices = c("0", "10", "20"),
				inline = TRUE
			),
			checkboxGroupInput(
				"filter_n_patients_per_arm",
				"Patients per arm",
				selected = c("100", "500", "1000", "2000"),
				choices = c("100", "500", "1000", "2000"),
				inline = TRUE
			)
		),
		box(
			title = "Apperance",
			width = 12,
			collapsed = TRUE,
			collapsible = TRUE,
			solidHeader = TRUE,
			numericInput("point_size", "Point size", value = 2, min = 0, step = 0.25),
			numericInput("plot_text_size", "Text size", value = 10, min = 1, step = 1),
			numericInput(
				"strat_label_size",
				"Text size for stratitification labels",
				value = 9,
				min = 1,
				step = 1
			),
			textInput("brewer_style", "ColorBrewer palette (name)", value = "Set1"),
			numericInput(
				"relative_plot_height",
				"Plot height (% of window height)",
				value = 90,
				min = 0,
				step = 5
			),
			sliderInput(
				"plot_bars_ratio",
				"Ratio between the two sub-plots",
				min = 1,
				max = 10,
				value = 4,
				pre = "1-to-",
				step = 1,
				ticks = FALSE
			)
		)
	),
	br(),
	actionButton(
		"see_source_code",
		"Take me to the source code!",
		class = "btn-warning",
		icon = icon("brands fa-github"),
		onclick ="window.open('https://github.com/epiben/aqua-icu-explorer', '_blank')",
		width = "270"
	)
)

body <- dashboardBody(
	uiOutput("main_panel")
)

ui <- dashboardPage(
	dashboardHeader(title = "AQUA-ICU explorer"),
	sidebar,
	body,
	skin = "yellow"
)

ui
