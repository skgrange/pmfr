#' Function to replace missing and negative values in concentration and error
#' tables in preparation for use with the EPA-PMF tool. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @param df Data frame containing concentration data. 
#' 
#' @param df_errors Data frame containing errror or uncertainty data. 
#' 
#' @param zero_replace Replacement value for values which are <= 0. 
#' 
#' @param na_replace Replacement value for missing values (NA). 
#' 
#' @return Named list containing two tibbles. 
#' 
#' @export
replace_missing_and_negative_values <- function(df, df_errors, 
                                                zero_replace = 1e-06, 
                                                na_replace = 10) {
  
  # To-do: argument for function to use for imputing
  
  # Back to base data frame for index work
  df <- data.frame(df)
  df_errors <- data.frame(df_errors)
  
  # Find indices
  index_missing <- is.na(df)
  index_less_than_zero <- df <= 0 & !is.na(df)
  
  # Manipulate concentration values
  df <- df %>% 
    dplyr::mutate_if(is.numeric, ~tidyr::replace_na(., median(., na.rm = TRUE))) %>% 
    dplyr::mutate_if(is.numeric, ~if_else(. <= 0, zero_replace, .))
  
  # Manipulate error table
  # Use concentration table here
  df_errors[index_missing] <- as.numeric(df[index_missing]) * na_replace
  
  # Just push values
  # Use concentration data
  df_errors[index_less_than_zero] <- na_replace
  
  # Evaluate errors
  df_errors <- df_errors %>% 
    dplyr::mutate_if(is.numeric, ~if_else(. <= 0, zero_replace, .))
  
  # Build list return
  list_df <- list(
    concentrations = as_tibble(df),
    errors = as_tibble(df_errors)
  )
  
  return(list_df)
  
}
