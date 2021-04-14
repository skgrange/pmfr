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
  
  # Return empty tibble if no file is passed
  if (length(file) == 0) return(tibble())
  
  # Read as character vector
  text <- readr::read_lines(file)
  
  # Drop missing lines
  text_filter <- text[text != ""]
  text_filter <- stringr::str_subset(text_filter, "^Factor Profiles", negate = TRUE)
  
  index_start <- stringr::str_which(text_filter, "^,,")
  index_end <- dplyr::lead(index_start) - 1L
  index_end <- if_else(is.na(index_end), length(text_filter), index_end)
  
  # Split into pieces then parse the tabular data
  # Message suppression is for missing variable names
  suppressWarnings(
    df <- purrr::map2(index_start, index_end, ~text_filter[.x:.y]) %>% 
      purrr::map_dfr(readr::read_csv, na = "*", .id = "table") %>% 
      mutate(table = as.integer(table))
  )
  
  # Clean names
  names(df)[2:3] <- c("model_run", "species")
  names(df)[-2:-3] <- stringr::str_to_lower(names(df)[-2:-3])
  names(df)[-2:-3] <- stringr::str_replace_all(names(df)[-2:-3], " ", "_")
  
  # Decode factor profiles
  df <- df %>% 
    tibble::rowid_to_column() %>% 
    mutate(model_run_lag = dplyr::lag(model_run),
           model_run_lag = if_else(is.na(model_run_lag), 2, model_run_lag),
           model_run_delta = model_run_lag - model_run)
  
  # For a single model run
  if (length(unique(df$table)) == 3) {
    
    df <- df %>% 
      mutate(
        factor_profile = dplyr::case_when(
          table == 1 ~ "concentration_of_species",
          table == 2 ~ "percentage_of_species_sum",
          table == 3 ~ "percentage_of_factor_total"
        )
      )
    
  } else {
    
    # Isolate start of new factor profile types
    # Modulo could be used on "table" here, but results in false positives
    df_factor_profiles <- df %>% 
      filter(model_run_delta > 0) %>% 
      select(rowid) %>% 
      mutate(
        factor_profile = c(
          "concentration_of_species", "percentage_of_species_sum",
          "percentage_of_factor_total"
        )
      )
    
    # Join start of new factor profile types and push forward
    df <- df %>% 
      left_join(df_factor_profiles, by = "rowid") %>% 
      tidyr::fill(factor_profile) 
    
  }
  
  # Order the variables
  df <- df %>% 
    mutate(model_type = "base") %>% 
    select(model_type,
           factor_profile,
           model_run,
           species,
           dplyr::starts_with("factor_"))

  return(df)
  
}


#' Function to reshape PMF factor profiles into tidy data. 
#' 
#' @param df Tibble/data frame from \code{\link{read_pmf_factor_profiles}}. 
#' 
#' @param factor_to_integer Should the factor variable be made an integer?
#' 
#' @param x A string in the format \code{"factor_*"} to be made an integer. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
tidy_pmf_profiles <- function(df, factor_to_integer = FALSE) {
  
  # Set id variables
  id_variables <- c("factor_profile", "model_run", "species")
  
  # Add the extra if it exists, this variable was not included in the past
  if ("model_type" %in% names(df)) id_variables <- c("model_type", id_variables)
  
  # Make the table longer
  df <- df %>% 
    tidyr::pivot_longer(-dplyr::all_of(id_variables), names_to = "factor") %>% 
    arrange(factor,
            factor_profile, 
            species)
  
  # Remove prefix and make factor an integer
  if (factor_to_integer) {
    df <- mutate(df, factor = str_factor_to_integer(factor))
  }
  
  return(df)
  
}


#' @rdname tidy_pmf_profiles
#' @export
str_factor_to_integer <- function(x) {
  x %>% 
    stringr::str_remove("^factor_") %>% 
    as.integer()
}
