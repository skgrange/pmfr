#' Function to read PMF run comparisions exported from the EPA PMF tool. 
#' 
#' @param file File to read. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_pmf_run_comparisons <- function(file) {
  
  # Read as text
  text <- readr::read_lines(file)
  
  # Get the three different types of statistics
  df <- bind_rows(
    read_pmf_run_comparisons_concentrations(text),
    read_pmf_run_comparisons_species_sum(text),
    read_pmf_run_comparisons_total(text)
  )
  
  # Give good names
  names(df)[-1:-2] <- c(
    "species", "lowest_q", "minimum", "lower_quartile", "median", 
    "upper_quartile", "maximum", "mean", "standard_deviation", 
    "rsd_percent_mean", "rsd_percent_lowest_mean"
  )
  
  return(df)
  
}


read_pmf_run_comparisons_concentrations <- function(text) {
  
  # Isolate table
  index_start <- stringr::str_which(text, "Concentration of Species") + 1
  index_end <- stringr::str_which(text, "Percent of Species") - 1
  
  # Filter text to tables
  text_filter <- text[index_start:index_end]
  
  index_start_table <- stringr::str_which(text_filter, "Factor") + 1L
  index_end_table <- dplyr::lead(index_start_table) - 2L
  index_end_table <- if_else(is.na(index_end_table), length(text_filter), index_end_table)
  
  df <- read_pmf_run_comparisons_tables(
    text_filter, 
    index_start = index_start_table, 
    index_end = index_end_table, 
    comparison = "concentration_of_species"
  )
  
  return(df)
  
}


read_pmf_run_comparisons_species_sum <- function(text) {
  
  # Isolate table
  index_start <- stringr::str_which(text, "Percent of Species") + 1
  index_end <- stringr::str_which(text, "Percent of Total") - 1
  
  # Filter text to tables
  text_filter <- text[index_start:index_end]
  
  index_start_table <- stringr::str_which(text_filter, "Factor") + 1L
  index_end_table <- dplyr::lead(index_start_table) - 2L
  index_end_table <- if_else(is.na(index_end_table), length(text_filter), index_end_table)
  
  df <- read_pmf_run_comparisons_tables(
    text_filter, 
    index_start = index_start_table, 
    index_end = index_end_table, 
    comparison = "percent_of_species_sum"
  )
  
  return(df)
  
}


read_pmf_run_comparisons_total <- function(text) {
  
  # Isolate table
  index_start <- stringr::str_which(text, "Percent of Total") + 1
  
  # Filter text to tables
  text_filter <- text[index_start:length(text)]
  
  index_start_table <- stringr::str_which(text_filter, "Factor") + 1L
  index_end_table <- dplyr::lead(index_start_table) - 2L
  index_end_table <- if_else(is.na(index_end_table), length(text_filter), index_end_table)
  
  df <- read_pmf_run_comparisons_tables(
    text_filter, 
    index_start = index_start_table, 
    index_end = index_end_table, 
    comparison = "percent_of_total_variable"
  )
  
  return(df)
  
}


read_pmf_run_comparisons_tables <- function(text, index_start, index_end, 
                                            comparison) {
  
  # Message supression is for missing column name
  df <- suppressWarnings(
    purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
      purrr::map_dfr(readr::read_csv, .id = "factor") %>% 
      mutate(factor = as.integer(factor),
             comparison = !!comparison) %>% 
      select(comparison, 
             factor,
             everything())
  )
  
  return(df)
  
}
