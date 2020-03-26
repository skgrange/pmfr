#' Function to list files/paths of the outputs created by the EPA PMF tool. 
#' 
#' @param directory Directory where EPA PMF files are located.
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list containing \strong{fs} paths. 
#' 
#' @export
list_epa_pmf_outputs <- function(directory) {
  
  # List all files
  file_list <- list.files(directory, full.names = TRUE)
  
  # Filter list to files
  file_diagnostics <- file_list %>% 
    stringr::str_subset("diagnostics.csv") %>% 
    stringr::str_subset("Fpeak", negate = TRUE)
  
  file_residuals <- file_list %>% 
    stringr::str_subset("residuals.csv")
  
  file_factor_profiles <- file_list %>% 
    stringr::str_subset("profiles.csv")
  
  file_contributions <- file_list %>% 
    stringr::str_subset("contributions.csv")
  
  file_run_comparisons <- file_list %>% 
    stringr::str_subset("comparison.csv")
  
  file_source_contributions <- file_list %>% 
    stringr::str_subset("sourcecontributions.xls")
  
  file_f_peak_diagnostics <- file_list %>% 
    stringr::str_subset("Fpeak_diagnostics")
  
  file_f_peak_error_estimates <- file_list %>% 
    stringr::str_subset("peakErrorEstimation")
  
  # Build list
  list_files <- list(
    diagnostics = file_diagnostics,
    residuals = file_residuals,
    factor_profiles = file_factor_profiles,
    contributions = file_contributions,
    run_comparisons = file_run_comparisons,
    source_contributions = file_source_contributions,
    f_peak_diagnostics = file_f_peak_diagnostics,
    f_peak_error_estimates = file_f_peak_error_estimates
  ) %>% 
    purrr::map(fs::as_fs_path)
  
  return(list_files)
  
}
