#' Function to read constrained PMF diagnostics exported from the EPA PMF tool. 
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
read_pmf_constrained_diagnostics <- function(file, tz = "UTC") {
  
  # Return empty list if no file is passed
  if (length(file) == 0) return(list())
  
  # Read file as text
  text <- readr::read_lines(file)
  
  # Build a named list with all the components
  # To-do: the empty tibbles are for tables which are to be formatted...
  list_components <- list(
    analysis_summary = format_analysis_summary_constrained(text),
    constrained_run_summary = format_constrained_run_summary(text),
    expressions = tibble(),
    constraints = format_constrained_constraints(text),
    constrained_run_summary_table = format_constrained_run_summary_table(text),
    constrained_factor_profiles = format_constrained_factor_profiles(text),
    constrained_factor_contributions = format_constrained_factor_contributions(text, tz = tz),
    residuals = tibble(), 
    regression_diagnostics = tibble()
  )
  
  return(list_components)
  
}


format_analysis_summary_constrained <- function(text) {
  
  # Where does the unit start and end? 
  index_start <- stringr::str_which(text, "Analysis Summary") + 2L
  index_end <- stringr::str_which(text, "Constrained Run Summary")[1] - 3L
  
  # Get unit and clean
  text[index_start:index_end] %>% 
    stringr::str_split_fixed(",", 2) %>% 
    as_tibble(.name_repair = "minimal") %>% 
    purrr::set_names(c("variable", "value")) %>% 
    mutate(variable = stringr::str_remove(variable, ":"),
           variable = stringr::str_trim(variable), 
           variable = str_to_underscore(variable),
           value = stringr::str_replace_all(value, "\\\\", "/")) %>% 
    tidyr::pivot_wider(names_from = variable) %>% 
    mutate(time_of_run = lubridate::mdy_hm(time_of_run, tz = "UTC"))
  
}


format_constrained_run_summary <- function(text) {
  
  # Where does the unit start and end? 
  index_start <- stringr::str_which(text, "Constrained Run Summary")[1] + 2L
  index_end <- stringr::str_which(text, "Expressions") - 2L
  
  text[index_start:index_end] %>% 
    stringr::str_split_fixed(",", 2) %>% 
    as_tibble(.name_repair = "minimal") %>% 
    purrr::set_names(c("variable", "value")) %>% 
    mutate(value = as.numeric(value),
           variable = stringr::str_trim(variable),
           variable = stringr::str_remove(variable, ":$"),
           variable = str_rm_round_brackets(variable),
           variable = str_to_underscore(variable)) %>% 
    tidyr::pivot_wider(names_from = variable)
  
}


format_constrained_constraints <- function(text) {
  
  # Where does the unit start and end? 
  index_start <- stringr::str_which(text, "Constraints:") + 1L
  index_end <- stringr::str_which(text, "Constrained Run Summary")[2] - 2L
  
  # Read table
  readr::read_csv(text[index_start:index_end]) %>% 
    dplyr::rename_all(str_to_underscore) %>% 
    rename(percent_d_q = `%_d_q`)
  
}


format_constrained_run_summary_table <- function(text) {
  
  # Where does the unit start and end? 
  index_start <- stringr::str_which(text, "Constrained Run Summary")[2] + 2L
  index_end <- stringr::str_which(text, "Factor Profiles")[1] - 2L
  
  # Parse table
  readr::read_csv(text[index_start:index_end]) %>% 
    purrr::set_names(
      c(
        "constrained_number", "d_q_robust", "q_robust", "q_aux", "q_true", 
        "converged", "number_of_steps"
      )
    ) %>% 
    mutate(converged = converged == "Yes",
           across(c("constrained_number", "number_of_steps"), as.integer))
  
}


format_constrained_factor_profiles <- function(text) {
  
  # Where does the unit start and end? 
  index_start <- stringr::str_which(text, "Factor Profiles")[1] + 1L
  index_end <- stringr::str_which(text, "Factor Contributions")[1] - 2L
  
  # Parse table, suppression is for missing rows
  suppressWarnings(
    df <- readr::read_csv(text[index_start:index_end], col_names = FALSE, na = "*")
  )
  
  # Clean the names
  # Determine number of factors and make their names
  n_factors <- seq_len(length(df) - 2L)
  variable_names_factors <- stringr::str_c("factor_", n_factors)
  
  # Combine with identifiers
  variable_names <- c("model_run", "species", variable_names_factors)
  
  # Give names, could be done in pipeline
  names(df) <- variable_names
  
  # Add factor profile identifier and clean table a bit
  df <- df %>%
    tibble::rowid_to_column() %>% 
    mutate(
      factor_profile = if_else(rowid == 1L, "concentration_of_species", NA_character_),
      factor_profile = dplyr::case_when(
        stringr::str_detect(model_run, "% of species sum") ~ "percentage_of_species_sum",
        stringr::str_detect(model_run, "% of total variable") ~ "percentage_of_factor_total",
        TRUE ~ factor_profile
      ),
      factor_profile = na_locf(factor_profile)
    ) %>% 
    select(-rowid) %>% 
    filter(!stringr::str_detect(model_run, "^Factor")) %>% 
    mutate(model_run = as.integer(model_run),
           model_type = "constrained") %>% 
    relocate(model_type,
             factor_profile)
  
  return(df)
  
}


format_constrained_factor_contributions <- function(text, tz) {
  
  # Where does the unit start and end? 
  index_start <- stringr::str_which(text, "Factor Contributions")[1] + 2L
  index_end <- stringr::str_which(text, "Residuals from")[1] - 2L
  
  # Parse table, suppression is for missing rows
  suppressWarnings(
    df <- readr::read_csv(text[index_start:index_end], col_names = FALSE)
  )
  
  # Clean the names
  # Determine number of factors and make their names
  n_factors <- seq_len(length(df) - 3L)
  variable_names_factors <- stringr::str_c("factor_", n_factors)
  
  # Combine with identifiers
  variable_names <- c("model_run", "id", "date", variable_names_factors)
  
  # Give names, could be done in pipeline
  names(df) <- variable_names
  
  # Add unit identifier and clean data a bit
  df <- df %>% 
    tibble::rowid_to_column() %>% 
    mutate(
      unit = dplyr::case_when(
        rowid == 1 ~ "normalised",
        stringr::str_detect(model_run, "Total Variable") ~ "concentrations",
        TRUE ~ NA_character_
      ),
      unit = na_locf(unit)
    ) %>% 
    select(-rowid) %>% 
    filter(!stringr::str_detect(model_run, "Factor|Total")) %>% 
    mutate(date = lubridate::mdy_hms(date, tz = tz, truncated = 3),
           model_run = as.integer(model_run), 
           model_type = "constrained") %>% 
    relocate(model_type,
             model_run,
             unit)
  
  return(df)
  
}
