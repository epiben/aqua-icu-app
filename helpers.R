library(shiny)
library(shinydashboard)
library(shinyBS)
library(magrittr)
library(data.table)
library(qs)
library(stringr)

library(ggplot2)
library(patchwork)

comparisons <- qread("inst/comparisons.qs")

all_strat_vars <- c( # all variables by which to be stratified
	"acceleration_hrqol",
	"prop_mortality_benefitters",
	"mortality",
	"final_hrqol",
	"n_patients_per_arm"
)

performance_metrics <- c(
	"rejection_proportion",
	"bias",
	"relative_bias"
)

beautify_labels <- c(
	"0 vs. 0" = "0",
	"0.02 vs. 0" = "2",
	"0.05 vs. 0" = "5",
	"0.1 vs. 0" = "10",
	"0.15 vs. 0" = "15",
	"0.36 vs. 0.4" = "10",
	"0.38 vs. 0.4" = "5",
	"0.39 vs. 0.4" = "2.5",
	"0.4 vs. 0.4" = "0",
	"mortality reduction" = "Mortality reduction (%)",
	"prop_mortality_benefitters" = "Mortality benefitters (%)",
	"acceleration_hrqol" = "Acceleration in HRQoL (%)"
)
