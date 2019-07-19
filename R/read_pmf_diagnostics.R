#' Function to read PMF diagnostics exported from the EPA PMF tool. 
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
read_pmf_diagnostics <- function(file, tz = "UTC") {
  
  # Read file as text
  text <- readr::read_lines(file)
  
  # Build list
  list_components <- list(
    analysis_summary = format_analysis_summary(text),
    input_data_statistics = format_input_data_statistics(text),
    base_run_summary = format_base_run_summary(text),
    base_run_summary_table = format_base_run_summary_table(text),
    scaled_residual_analysis = format_scaled_residual_analysis(text),
    scaled_residual_analysis_sum_d = format_scaled_residual_analysis_sum_d(text),
    ks_test = format_ks_test(text),
    scaled_residuals_by_species = format_scaled_residuals_by_species(text, tz = tz),
    scaled_residuals_by_date = format_scaled_residuals_by_date(text, tz = tz)
  )
  
  return(list_components)
  
}



format_analysis_summary <- function(text) {
  
  index_start <- stringr::str_which(text, "Analysis Summary") + 1
  index_end <- stringr::str_which(text, "Input Data Statistics") - 1
  
  # Split
  matrix_text <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    stringr::str_split_fixed(":", 2) 
  
  matrix_text[, 1] <- matrix_text[, 1] %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" ", "_")
  
  matrix_text[, 2] <- matrix_text[, 2] %>% 
    stringr::str_trim() %>% 
    stringr::str_remove("^,")
  
  df <- matrix_text[, 2] %>% 
    purrr::set_names(matrix_text[, 1]) %>% 
    tibble::enframe() %>% 
    tidyr::spread(name, value) %>% 
    dplyr::mutate_all(~if_else(. == "", NA_character_, .)) %>% 
    mutate(time_of_run = lubridate::mdy_hm(time_of_run, tz = "UTC"))
  
  return(df)
  
}


format_input_data_statistics <- function(text) {
  
  index_start <- stringr::str_which(text, "Input Data Statistics") + 1
  index_end <- stringr::str_which(text, "Excluded Samples") - 1
  
  df <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    readr::read_csv()
  
  return(df)
  
}


format_base_run_summary <- function(text) {
  
  index_start <- stringr::str_which(text, "Base Run Summary") + 1
  index_end <- stringr::str_which(text, "Base run summary table") - 1
  
  matrix_text <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    stringr::str_split_fixed(":", 2) 
  
  matrix_text[, 1] <- matrix_text[, 1] %>% 
    stringr::str_replace_all("\\s*\\([^\\)]+\\)", "") %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" |-", "_") %>% 
    stringr::str_remove("_$")
  
  matrix_text[, 2] <- matrix_text[, 2] %>% 
    stringr::str_trim() %>% 
    stringr::str_remove("^,")
  
  df <- matrix_text[, 2] %>% 
    purrr::set_names(matrix_text[, 1]) %>% 
    tibble::enframe() %>% 
    tidyr::spread(name, value, convert = TRUE)
  
  return(df)
  
}


format_base_run_summary_table <- function(text) {
  
  index_start <- stringr::str_which(text, "Base run summary table") + 1
  index_end <- stringr::str_which(text, "Scaled residual analysis") - 1
  
  df <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    readr::read_csv() %>% 
    purrr::set_names(
      c("model_run", "q_robust", "q_true", "converged", "number_of_steps", 
        "q_true_q_exp")
    ) %>% 
    mutate(converged = if_else(converged == "Yes", TRUE, FALSE))
  
  return(df)
  
}


format_ks_test <- function(text) {
  
  # To-do, vectorise this
  
  index_start <- stringr::str_which(text, "Regression diagnostics") + 1
  index_end <- stringr::str_which(text, "Scaled residuals beyond")[1] - 1
  
  df <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    readr::read_csv(skip = 1)
  
  return(df)
  
}


format_scaled_residual_analysis <- function(text) {
  
  index_start <- stringr::str_which(text, "Scaled residual analysis") + 1
  index_end <- stringr::str_which(text, "Sum of d across") - 1
  
  # Supression is for an empty column
  suppressWarnings(
    df <- text[index_start:index_end] %>% 
      .[. != ""] %>% 
      readr::read_csv(skip = 1) %>% 
      dplyr::select_if(Negate(is.logical))
  )
  
  names(df)[1] <- "model_run"
  
  return(df)
  
}


format_scaled_residual_analysis_sum_d <- function(text) {
  
  index_start <- stringr::str_which(text, "Sum of d across") + 1
  index_end <- stringr::str_which(text, "Analysis of Base Run") - 1
  
  # Supression is for an empty column
  suppressWarnings(
    df <- text[index_start:index_end] %>% 
      .[. != ""] %>% 
      readr::read_csv() %>% 
      dplyr::select_if(Negate(is.logical))
  )
  
  names(df)[1:2] <- c("model_run", "model_run_two")

  return(df)
  
}


format_scaled_residuals_by_species <- function(text, tz) {
  
  # To-do, vectorise this
 
  index_start <- stringr::str_which(text, "dates by species") + 1
  index_end <- stringr::str_which(text, "species by date") - 1
  
  df <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    readr::read_csv() %>% 
    purrr::set_names(c("species", "date", "residual")) %>% 
    mutate(date = lubridate::mdy_hm(date, tz = tz))
  
  return(df)
  
}


format_scaled_residuals_by_date <- function(text, tz) {
  
  # To-do, vectorise this
  
  index_start <- stringr::str_which(text, "species by date") + 1
  
  df <- text[index_start:length(text)] %>% 
    .[. != ""] %>% 
    readr::read_csv() %>% 
    purrr::set_names(c("date", "species", "residual")) %>% 
    mutate(date = lubridate::mdy_hm(date, tz = tz))
  
  return(df)
  
}
