#' Function to read PMF base error estimation summarries exported from the EPA
#' PMF tool. 
#' 
#' @param file File to read. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_pmf_base_error_estimations <- function(file) {
  
  # Read file as text
  text <- readr::read_lines(file)
  
  # Remove trailing commas
  text <- stringr::str_remove(text, ",$")
  
  list_pmf <- list(
    displaced_species = read_pmf_base_error_estimations_displaced(text),
    diagnostics = read_pmf_base_error_estimations_diagnostics(text),
    error_estimations = bind_rows(
      read_pmf_base_error_estimations_concentrations(text),
      read_pmf_base_error_estimations_percent_sum(text),
      read_pmf_base_error_estimations_percent_total(text)
    )
  )
  
  return(list_pmf)
  
}


read_pmf_base_error_estimations_displaced <- function(text) {
  
  index_start <- stringr::str_which(text, "DISP Displaced Species") + 1L
  index_end <- stringr::str_which(text, "DISP Diagnostics") - 2L
  x <- text[index_start:index_end]
  x <- stringr::str_remove(x, "^,")
  return(x)
  
}


read_pmf_base_error_estimations_diagnostics <- function(text) {
  
  index_start <- stringr::str_which(text, "DISP Diagnostics") + 1L
  index_end <- stringr::str_which(text, "Concentrations for Factor")[1] - 2L
  
  df <- text[index_start:index_end] %>% 
    stringr::str_remove("^,") %>% 
    stringr::str_replace(":,", ":") %>% 
    stringr::str_split_fixed(":", 2) %>% 
    tibble::as_tibble(.name_repair = "minimal") %>% 
    purrr::set_names(c("variable", "value")) %>% 
    mutate(id = 1L) %>% 
    tidyr::spread(variable, value, convert = TRUE) %>% 
    select(-1) %>% 
    purrr::set_names(
      c("percent_d_q", "error_code", "largest_decrease_in_q", "swaps_by_factor")
    )
  
  return(df)
  
}


read_pmf_base_error_estimations_concentrations <- function(text) {
  
  # Isolate table
  index_start <- stringr::str_which(text, "Concentrations for Factor")[1]
  index_end <- stringr::str_which(text, "Percent of Species")[1] - 1
  
  # 
  text_filter <- text[index_start:index_end]
  
  index_start_table <- stringr::str_which(text_filter, "Concentrations") + 1L
  index_end_table <- dplyr::lead(index_start_table) - 2L
  index_end_table <- if_else(is.na(index_end_table), length(text_filter), index_end_table)
  
  # Filter text to concentration tables
  df <- read_pmf_base_error_estimations_tables(
    text = text_filter,
    index_start = index_start_table,
    index_end = index_end_table,
    comparison = "concentration_of_species"
  )
  
  return(df)
  
}


read_pmf_base_error_estimations_percent_sum <- function(text) {
  
  # Isolate table
  index_start <- stringr::str_which(text, "Percent of Species Sum")[1]
  index_end <- stringr::str_which(text, "Percent of Factor Total")[1] - 1
  
  # Isolate table
  text_filter <- text[index_start:index_end]
  
  index_start_table <- stringr::str_which(text_filter, "Percent of Species Sum") + 1L
  index_end_table <- dplyr::lead(index_start_table) - 2L
  index_end_table <- if_else(is.na(index_end_table), length(text_filter), index_end_table)
  
  # Filter text to concentration tables
  df <- read_pmf_base_error_estimations_tables(
    text = text_filter,
    index_start = index_start_table,
    index_end = index_end_table,
    comparison = "percent_of_species_sum"
  )
  
  return(df)
  
}


read_pmf_base_error_estimations_percent_total <- function(text) {
  
  # Isolate table
  index_start <- stringr::str_which(text, "Percent of Factor Total")[1]

  # Isolate table
  text_filter <- text[index_start:length(text)]
  
  index_start_table <- stringr::str_which(text_filter, "Percent of Factor Total") + 1L
  index_end_table <- dplyr::lead(index_start_table) - 2L
  index_end_table <- if_else(is.na(index_end_table), length(text_filter), index_end_table)
  
  # Filter text to concentration tables
  df <- read_pmf_base_error_estimations_tables(
    text = text_filter,
    index_start = index_start_table,
    index_end = index_end_table,
    comparison = "percent_of_factor_total"
  )
  
  return(df)
  
}


read_pmf_base_error_estimations_tables <- function(text, index_start, index_end,
                                                  comparison) {
  
  df <- purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
    purrr::map_dfr(readr::read_csv, .id = "factor") %>% 
    mutate(factor = as.integer(factor),
           comparison = !!comparison) %>% 
    select(comparison,
           everything()) %>% 
    dplyr::mutate_if(is.logical, as.numeric)
  
  # Give name
  names(df)[-1:-2] <- c(
    "species", "base_value", "bootstrap_5th", "bootstrap_25th", "bootstrap_50th", 
    "bootstrap_75th", "bootstrap_95th", "bootstrap_disp_5th", "bootstrap_disp_average", 
    "bootstrap_disp_95th", "disp_minimum", "disp_average", "disp_maximum"
  )
  
  return(df)

}
