source("helpers.R")

# Define UI for application that draws a histogram
body <- dashboardBody(
	fluidRow(
		column(
			4,
			selectInput("shape_var", "Shape variable", all_strat_vars, multiple = FALSE, selected = "final_hrqol"),
			selectInput("colour_var", "Colouring variable", all_strat_vars, multiple = FALSE, selected = "n_patients_per_arm")
		),
		column(
			4,
			selectInput("y_var", "Variable on vertical axis", performance_metrics, multiple = FALSE, selected = "rejection_proportion")
		),
		column(
			4,
			textInput("brewer_style", "Brewer style", value = "Set1")
		)
	),

	fluidRow(
		plotOutput("the_plot")
	)
)

ui <- dashboardPage(
	dashboardHeader(title = "AQUA-ICU explorer"),
	dashboardSidebar(disable = TRUE),
	body
)

ui
