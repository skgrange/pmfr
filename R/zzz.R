#' Squash the global variable notes when building a package.
#'
if (getRversion() >= "2.15.1") {

  # What variables are causing issues?
  variables <- c(
    "factor_profile_type", "factor_profile", "value", "model_run", "variable", 
    ".", "Base_Run", "Date_Time", "name", "time_of_run", "model_run_lag", 
    "model_run_delta", "rowid", "converged", "concentration_file", 
    "uncertainty_file", "percent_modelled_samples", "percent_raw_samples",
    "category", "species", "site", "statistic", "...1", "...3", "X1", "X2",
    "f_peak_run", "base_run", "residual_type", "configuration_file",
    "bootstrap_factor", "error_estimation_type", "f_peak_number", 
    "number_of_steps", "contribution", "contribution_percent", "label", 
    "q_robust", "contribution_label", "method_type", "value_sum", "unit",
    "q_true", "list_diagnostics", "Sample_IDs", "%_d_q", "model_type"
  )

  # Squash the notes
  utils::globalVariables(variables)

}
