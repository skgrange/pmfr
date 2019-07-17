#' Squash the global variable notes when building a package.
#'
if (getRversion() >= "2.15.1") {

  # What variables are causing issues?
  variables <- c(
    "factor_profile_type", "factor_profile", "value", "model_run", "variable", 
    ".", "Base_Run", "Date_Time", "name", "time_of_run"
  )

  # Squash the notes
  utils::globalVariables(variables)

}
