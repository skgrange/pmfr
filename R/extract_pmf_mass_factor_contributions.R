#' Function to extract PMF factor contributions from the return of
#' \code{\link{read_pmf_factor_profiles}}. 
#' 
#' @param df Tibble from \code{\link{read_pmf_factor_profiles}}. 
#' 
#' @param value_mass An optional value to use for fully accounted for mass in 
#' the calculations. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{read_pmf_factor_profiles}}, 
#' \code{\link{plot_pmf_mass_factor_contributions}}
#' 
#' @export 
extract_pmf_mass_factor_contributions <- function(df, value_mass = NA) {
  
  # Check input
  stopifnot(length(value_mass) == 1)
  
  # Filter table 
  df <- df %>% 
    filter(factor_profile == "concentration_of_species",
           species == "mass")
  
  if (!is.na(value_mass[1])) {
    
    # Calculate missing mass with the use of input
    df_sums <- df %>% 
      group_by(factor_profile,
               model_run,
               species) %>% 
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(value = !!value_mass - value,
             factor = "missing")
    
    # Bind
    df <- bind_rows(df, df_sums)
    
  }
  
  # Calculate sums and contribution
  df <- df %>% 
    group_by(factor_profile,
             model_run,
             species) %>% 
    mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(contribution = value / value_sum)
  
  # Raise warning if negative
  if (any(df$value < 0)) {
    warning("Negative masses detected...", call. = FALSE)
  }
  
  return(df)
  
}
