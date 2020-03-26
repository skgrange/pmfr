#' Function to read PMF F-peak diagnostics exported from the EPA PMF tool. 
#' 
#' @param file File to read. 
#' 
#' @param tz Time zone the dates are stored in. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list containing tibbles. 
#' 
#' @export
read_pmf_f_peak_diagnostics <- function(file, tz = "UTC") {
  
  # Load file as text
  text <- readr::read_lines(file)
  
  list_data <- list(
    analysis_summary = read_pmf_f_peak_diagnostics_analysis_summary(text),
    run_summary = read_pmf_f_peak_diagnostics_run_summary(text),
    run_summary_table = read_pmf_f_peak_diagnostics_run_summary_table(text),
    factor_profiles = read_pmf_f_peak_diagnostics_factor_profiles(text),
    factor_contributions = read_pmf_f_peak_diagnostics_factor_contributions(text, tz = tz),
    regression_diagnostics = read_pmf_f_peak_diagnostics_regression_diagnostics(text)
  )
  
  return(list_data)
  
}


read_pmf_f_peak_diagnostics_analysis_summary <- function(text) {
  
  index_start <- stringr::str_which(text, "Analysis Summary") + 1L
  index_end <- stringr::str_which(text, "Fpeak Run")[1] - 1L
  
  df <- text[index_start:index_end] %>% 
    readr::read_csv(col_names = FALSE) %>% 
    rename(variable = X1,
           value = X2) %>% 
    mutate(variable = clean_summary_variables(variable),
           value = stringr::str_replace_all(value, "\\\\", "/"),
           file = 1L) %>% 
    tidyr::spread(variable, value) %>% 
    select(-file) %>% 
    mutate(time_of_run = lubridate::mdy_hm(time_of_run, tz = "UTC"))
  
  return(df)
  
}


read_pmf_f_peak_diagnostics_run_summary <- function(text) {
  
  index_start <- stringr::str_which(text, "Fpeak Run Summary")[1] + 1L
  index_end <- stringr::str_which(text, "Fpeak Run Summary Table") - 1L
  
  df <- text[index_start:index_end] %>% 
    readr::read_csv(col_names = FALSE) %>% 
    rename(variable = X1,
           value = X2) %>% 
    mutate(variable = clean_summary_variables(variable),
           variable = stringr::str_replace(variable, "\\(%\\)", "percent"))
  
  return(df)
  
}


read_pmf_f_peak_diagnostics_run_summary_table <- function(text) {
  
  index_start <- stringr::str_which(text, "Fpeak Run Summary Table") + 1L
  index_end <- stringr::str_which(text, "Factor Profiles")[1] - 1L
  
  df <- text[index_start:index_end] %>% 
    readr::read_csv() %>% 
    purrr::set_names(
      c(
        "f_peak_run", "strength", "d_q_robust", "q_robust", "d_q_robust_precent",
        "q_aux", "q_true", "converged", "number_of_steps"
      )
    ) %>% 
    dplyr::mutate_all(type.convert, as.is = TRUE) %>% 
    mutate(converged = if_else(converged == "Yes", TRUE, FALSE))
  
  return(df)
  
}


read_pmf_f_peak_diagnostics_factor_profiles <- function(text) {
  
  # Isolate unit
  index_start <- stringr::str_which(text, "Factor Profiles")[1]
  index_end <- stringr::str_which(text, "Factor Contributions")[1] - 1L
  text_filter <- text[index_start:index_end]
  
  # Get tables
  index_start_tables <- stringr::str_which(text_filter, "Factor Profiles") + 1L
  index_end_tables <- dplyr::lead(index_start_tables) - 2L
  index_end_tables <- if_else(is.na(index_end_tables), length(text_filter), index_end_tables)
  
  # Make tibble
  df <- purrr::map2(index_start_tables, index_end_tables, ~text_filter[.x:.y]) %>% 
    purrr::map_dfr(readr::read_csv, col_names = FALSE, .id = "factor_profile") %>% 
    rename(f_peak_run = X1,
           species = X2) %>% 
    mutate(factor_profile = as.integer(factor_profile),
           f_peak_run = as.integer(f_peak_run),
           factor_profile = dplyr::case_when(
             factor_profile == 1 ~ "concentration_of_species", 
             factor_profile == 2 ~ "percentage_of_species_sum", 
             factor_profile == 3 ~ "percentage_of_factor_total")
    )
  
  # Give factor names
  factor_names <- stringr::str_c("factor_", seq(1, ncol(df) - 3L))
  names(df)[-1:-3] <- factor_names
  
  return(df)
  
}


read_pmf_f_peak_diagnostics_factor_contributions <- function(text, tz) {
  
  # Isolate unit
  index_start <- stringr::str_which(text, "Factor Contributions") + 1L
  index_regression_diagnostics <- stringr::str_which(text, "Regression diagnostics")[1] - 1L
  index_end <- dplyr::lead(index_start) - 2L
  index_end <- if_else(is.na(index_end), index_regression_diagnostics, index_end)
  
  df <- purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
    purrr::map_dfr(readr::read_csv, , col_names = FALSE, .id = "model_run") %>% 
    rename(f_peak_run = X1,
           date = X2) %>% 
    mutate(f_peak_run = as.integer(f_peak_run),
           date = lubridate::mdy_hms(date, tz = tz, truncated = 3))
  
  # Give factor names
  factor_names <- stringr::str_c("factor_", seq(1, ncol(df) - 3L))
  names(df)[-1:-3] <- factor_names
  
  return(df)
  
}


read_pmf_f_peak_diagnostics_regression_diagnostics <- function(text) {
  
  index_start <- stringr::str_which(text, "Regression diagnostics") + 2L
  index_end <- dplyr::lead(index_start) - 3L
  index_end <- if_else(is.na(index_end), length(text), index_end)
  
  df <- purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
    purrr::map_dfr(readr::read_csv, .id = "model_run") %>% 
    mutate(model_run = as.integer(model_run)) %>% 
    purrr::set_names(
      c(
        "model_run", "species", "intercept", "slope", "standard_error", "r_squared", 
        "statistic", "p_value"
      )
    )
  
  return(df)
  
}


clean_summary_variables <- function(x) {
  
  x %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" |-", "_") %>% 
    stringr::str_remove(":$")
  
}
