#' Function to read PMF source contributions statistics exported from the EPA 
#' PMF tool. 
#'  
#' @param file File to read. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_pmf_source_contributions <- function(file) {
  
  # Read excel sheet, message supression is for name repair
  df <- suppressMessages(
    readxl::read_excel(
      file, 
      skip = 1,
      col_names = FALSE, 
      .name_repair = "unique",
      progress = FALSE
    )  
  )
  
  # Split table into the different runs
  index_start <- stringr::str_which(df$...1, "Lowest Q") + 2L
  index_end <- dplyr::lead(index_start) - 4L
  index_end <- if_else(is.na(index_end), ncol(df), index_end)
  
  # Split into the different runs
  df <- purrr::map2(index_start, index_end, ~dplyr::slice(df, .x:.y)) %>% 
    purrr::map_dfr(read_pmf_source_contributions_run_worker, .id = "model_run") %>% 
    mutate(model_run = as.integer(model_run))
  
  return(df)
  
}


read_pmf_source_contributions_run_worker <- function(df) {
  
  # Sort out identifiers and remove missing rows
  df <- df %>% 
    rename(site = ...1) %>% 
    filter(!is.na(site)) %>% 
    mutate(
      species = if_else(
        stringr::str_detect(site, "^Site", negate = TRUE), site, NA_character_)
    ) %>% 
    tidyr::fill(species) %>% 
    filter(!is.na(...3)) %>% 
    select(species,
           everything())
  
  # Get keys
  df_keys <- select(df, species, site)
  
  # Drop keys from statistics table
  df <- select(df, -species, -site)
  
  # Sort out names by building them
  factor_count <- sum(purrr::map_lgl(df, ~all(is.na(.)))) + 1L
  
  # The names of the statistics
  variable_names <- c(
    "n", "minimum", "lower_quartile", "median", "upper_quartile", "maximum",
    "mean", "standard_deviation", "interquartile_range", "missing"
  )
  
  # Create a squence to identify factor
  factor_count_sequence <- rep(1L:factor_count, each = length(variable_names))
  
  # Replicate variable names and add factor id
  variable_names <- variable_names %>% 
    rep(times = factor_count) %>% 
    stringr::str_c(factor_count_sequence, ";", .) %>% 
    .[-length(.)]
  
  # Give table names
  names(df) <- variable_names
  
  # Make tidy data, by adding keys, and reshaping
  df <- df %>% 
    select(-dplyr::ends_with("missing")) %>% 
    dplyr::bind_cols(df_keys) %>% 
    tidyr::gather(variable, value, -c(site, species)) %>% 
    tidyr::separate(col = variable, into = c("factor", "statistic"), sep = ";") %>% 
    mutate(value = as.numeric(value)) %>% 
    arrange(site, 
            factor,
            species,
            statistic)
  
  return(df)
  
}