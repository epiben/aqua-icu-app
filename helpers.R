library(shiny)
library(shinydashboard)
library(shinyBS)
library(magrittr)
library(stringr)
library(dplyr)
library(patchwork)
library(ggplot2)

all_strat_vars <- c( # all variables by which to be stratified
	"mortality" = "Mortality \nreduction (%)",
	"prop_mortality_benefitters" = "Mortality \nbenefitters (%)",
	"acceleration_hrqol" = "Acceleration \nin HRQoL (%)",
	"final_hrqol" = "Improved \nfinal HRQoL (%)",
	"n_patients_per_arm" = "Patients \nper arm"
)

comparisons <- as_tibble(qs::qread("inst/comparisons.qs"))

beautify_labels <- c(
	# acceleration_hrqol/prop_mortality_benefitters
	"0 vs. 0" = "0",
	"0.02 vs. 0" = "2",
	"0.05 vs. 0" = "5",
	"0.1 vs. 0" = "10",
	"0.15 vs. 0" = "15",
	# mortality
	"0.4 vs. 0.4" = "0",
	"0.39 vs. 0.4" = "2.5",
	"0.38 vs. 0.4" = "5",
	"0.36 vs. 0.4" = "10",
	# n_patients_per_arm
	"100 vs. 100" = "100",
	"500 vs. 500" = "500",
	"1000 vs. 1000" = "1000",
	"2000 vs. 2000" = "2000",
	# final_hrqol
	"0.75 vs. 0.75" = "0",
	"0.825 vs. 0.75" = "10",
	"0.9 vs. 0.75" = "20"
)

# Choices for select inputs
performance_metrics_choices <- c(
	"Rejection proportion" = "rejection_proportion",
	"Coverage of 95% CI" = "coverage",
	"Bias-corrected coverage of 95% CI" = "bias_corrected_coverage",
	"Bias" = "bias",
	"Relative bias" = "relative_bias",
	"Mean squared error" = "mse",

	"Std. error of rejection proportion" = "rejection_proportion_se",
	"Std. error of coverage of 95% CI" = "coverage_se",
	"Std. error of bias-corrected coverage of 95% CI" = "bias_corrected_coverage_se",
	"Std. error of bias" = "bias_se",
	"Std. error of relative bias" = "relative_bias_se",
	"Std. error of mean squared error" = "mse_se"
)

operationalisation_choices <- list(
	"HRQoL at end of follow-up" = "hrqol_at_eof",
	"Area under the trajectory" = "hrqol_auc"
)

analysis_choices <- list(
	"All participants" = "all",
	"Survivors only" = "survivors"
)

start_of_followup_choices <- list(
	"At ICU discharge" = "primary",
	"At hospital discharge" = "secondary1",
	"90 days after randomisation" = "secondary2"
)

shape_colour_choices <- as.list(
	setNames(names(all_strat_vars), all_strat_vars)
)

make_fct <- function(x) {
	level_idx <- names(beautify_labels) %in% unique(x)
	factor(x, names(beautify_labels[level_idx]), beautify_labels[level_idx])
}
