#' Function to adjust the \code{percentage_of_factor_total} entries from the
#' \code{\link{tidy_pmf_profiles}} output to ensure the factor sums equal 100 \%.
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame/tibble from \code{\link{tidy_pmf_profiles}}.
#' 
#' @return Tibble.
#' 
#' @export
adjust_pmf_percentage_profiles <- function(df) {
  
  # Check input
  name_test <- c(
    "model_type", "factor_profile", "model_run", "species", "factor", 
    "value"
  ) %in% names(df)
  
  stopifnot(all(name_test))
  
  # Filter to unit
  df <- filter(df, factor_profile == "percentage_of_total_variable")
  
  # Calculate the sum of the percentages
  df_sums <- df %>% 
    filter(factor_profile == "percentage_of_total_variable") %>% 
    group_by(model_type,
             factor_profile,
             model_run,
             factor) %>% 
    summarise(value_sum = sum(value, na.rm = TRUE),
              .groups = "drop")
  
  # Apply multiplier to make the species equal 100 percent
  df <- df %>% 
    left_join(
      df_sums,
      by = c("model_type", "factor_profile", "model_run", "factor")
    )   %>% 
    mutate(value = value * (1 / (value_sum / 100)),
           factor_profile = "percentage_of_total_variable_adjusted")
  
  return(df)
  
}
