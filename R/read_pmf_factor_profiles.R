#' Function to read PMF factor profiles exported from the EPA PMF tool. 
#' 
#' @param file File to read. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_pmf_factor_profiles <- function(file) {
  
  # Read as character vector
  text <- readr::read_lines(file)
  
  # Drop missing lines
  text_filter <- text[text != ""]
  text_filter <- stringr::str_subset(text_filter, "^Factor Profiles", negate = TRUE)
  
  index_start <- stringr::str_which(text_filter, "^,,")
  index_end <- dplyr::lead(index_start) - 1L
  index_end <- if_else(is.na(index_end), length(text_filter), index_end)
  
  # Split into pieces then parse the tabular data
  suppressWarnings(
    df <- purrr::map2(index_start, index_end, ~text_filter[.x:.y]) %>% 
      purrr::map_dfr(readr::read_csv, .id = "factor_profile_type")
  )
  
  # Clean names
  names(df)[2:3] <- c("model_run", "variable")
  names(df)[-1:-3] <- stringr::str_to_lower(names(df)[-1:-3])
  names(df)[-1:-3] <- stringr::str_replace_all(names(df)[-1:-3], " ", "_")
  
  # Decode factor profiles
  df <- df %>% 
    mutate(factor_profile = dplyr::case_when(
      factor_profile_type == 1 ~ "concentration_of_species",
      factor_profile_type == 2 ~ "percentage_of_species_sum",
      factor_profile_type == 3 ~ "percentage_of_factor_total",
    )) %>% 
    select(-factor_profile_type) %>% 
    select(factor_profile,
           everything())
  
  return(df)
  
}


#' Function to reshape PMF factor profiles into tidy data. 
#' 
#' @param df Tibble/data frame from \code{\link{read_pmf_factor_profiles}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
tidy_pmf_profiles <- function(df) {
  
  df %>% 
    tidyr::gather(factor, value, -c(factor_profile, model_run, variable)) %>% 
    arrange(factor,
            factor_profile, 
            variable)
  
}
