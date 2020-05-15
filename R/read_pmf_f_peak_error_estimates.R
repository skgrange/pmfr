#' Function to read PMF F-peak error estimations exported from the EPA PMF tool. 
#' 
#' @param file File to read. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list containing tibbles. 
#' 
#' @export
read_pmf_f_peak_error_estimates <- function(file) {
  
  # Return empty list if passed nothing
  if (length(file) == 0) return(list())
  
  # Read file as text
  text <- readr::read_lines(file)
  
  # Get a single value
  value_for_mapping <- stringr::str_subset(text, "Fpeak value for") %>% 
    stringr::str_split_fixed(",", 2) %>% 
    .[, 2] %>% 
    as.numeric()
  
  # Build list
  list_data <- list(
    value_for_mapping = value_for_mapping,
    error_estimation_summary = read_pmf_f_peak_error_estimation_summary(text),
    bootstrap_mapping = read_pmf_f_peak_error_estimation_summary_mapping(text),
    factor_contributions = bind_rows(
      read_pmf_f_peak_error_estimation_concentration(text),
      read_pmf_f_peak_error_estimation_precent_species(text),
      read_pmf_f_peak_error_estimation_precent_factor(text)
    )
  )
  
  return(list_data)
  
}


read_pmf_f_peak_error_estimation_summary <- function(text) {
  
  index_start <- stringr::str_which(text, "Fpeak Error Estimation Summary") + 1L
  index_end <- stringr::str_which(text, "Fpeak value for") - 1L
  
  df <- text[index_start:index_end]  %>% 
    readr::read_csv() %>% 
    purrr::set_names(
      c(
        "f_peak_run", "strength", "d_q_robust", "q_robust", "d_q_robust_percent", 
        "q_aux", "q_true", "converged", "number_of_steps"
      )
    ) %>% 
    mutate(f_peak_run = as.integer(f_peak_run),
           number_of_steps = as.integer(number_of_steps),
           converged = converged == "Yes")
  
  return(df)
  
}


read_pmf_f_peak_error_estimation_summary_mapping <- function(text) {
  
  index_start <- stringr::str_which(text, "^BS Mapping") + 1L
  index_end <- stringr::str_which(text, "Concentrations for Factor 1") - 1L
  
  # Warning suppression is for missing column name
  df <- suppressWarnings(
    text[index_start:index_end] %>% 
      readr::read_csv()
  )
  
  # Clean names
  names(df)[1] <- "bootstrap_factor"
  
  names(df)[-1] <- names(df)[-1] %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_replace_all("fpeak_", "f_peak_")
  
  # Clean table a bit
  df <- df %>% 
    mutate(bootstrap_factor = stringr::str_remove(bootstrap_factor, "Boot Factor ")) %>% 
    dplyr::mutate_all(type.convert, as.is = TRUE)
  
  return(df)
  
}


read_pmf_f_peak_error_estimation_concentration <- function(text) {
  
  # Get start and end indices
  index_start <- stringr::str_which(text, "Concentrations for") - 1L
  index_end_tables <- stringr::str_which(text, "Percent of Species Sum")[1] - 1L
  index_end <- dplyr::lead(index_start) - 1L
  index_end <- if_else(is.na(index_end), index_end_tables, index_end)
  
  # To tibble
  df <- purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
    purrr::map(~stringr::str_remove(., ",$")) %>% 
    purrr::map_dfr(readr::read_csv, skip = 2, .id = "factor") %>% 
    purrr::set_names(
      c(
        "factor", "species", "f_peak_value", "bootstrap_5th", "bootstap_median",
        "bootstrap_95th"
      )
    ) %>% 
    mutate(error_estimation_type = "concentration") %>% 
    select(error_estimation_type,
           everything())
  
  return(df)
  
}


read_pmf_f_peak_error_estimation_precent_species <- function(text) {
  
  # Get start and end indices
  index_start <- stringr::str_which(text, "Percent of Species Sum") - 1L
  index_end_tables <- stringr::str_which(text, "Percent of Factor")[1] - 1L
  index_end <- dplyr::lead(index_start) - 1L
  index_end <- if_else(is.na(index_end), index_end_tables, index_end)
  
  # To tibble
  df <- purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
    purrr::map(~stringr::str_remove(., ",$")) %>% 
    purrr::map_dfr(readr::read_csv, skip = 2, .id = "factor") %>% 
    purrr::set_names(
      c(
        "factor", "species", "f_peak_value", "bootstrap_5th", "bootstap_median",
        "bootstrap_95th"
      )
    ) %>% 
    mutate(error_estimation_type = "percent_of_species_sum") %>% 
    select(error_estimation_type,
           everything())
  
  return(df)
  
}


read_pmf_f_peak_error_estimation_precent_factor <- function(text) {
 
  # Get start and end indices
  index_start <- stringr::str_which(text, "Percent of Factor Total") - 1L
  index_end <- dplyr::lead(index_start) - 1L
  index_end <- if_else(is.na(index_end), length(text), index_end)
  
  # To tibble
  df <- purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
    purrr::map(~stringr::str_remove(., ",$")) %>% 
    purrr::map_dfr(readr::read_csv, skip = 2, .id = "factor") %>% 
    purrr::set_names(
      c(
        "factor", "species", "f_peak_value", "bootstrap_5th", "bootstap_median",
        "bootstrap_95th"
      )
    ) %>% 
    mutate(error_estimation_type = "percent_of_factor_total") %>% 
    select(error_estimation_type,
           everything())
  
  return(df)
   
}
