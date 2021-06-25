#' Function to calculate the species contribution to the factor.
#' 
#' This function is useful for when the contribution/percentage of an input
#' species is desired. \code{species_contributions_to_factor} will exclude the
#' "total variable" species (usually PM mass) from its calculation. 
#' 
#' @param df Data frame/tibble from \code{\link{tidy_pmf_profiles}}.
#' 
#' @param species_mass Name of the species which was used as the "total 
#' variable". 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
pmf_species_contributions_to_factor <- function(df, species_mass = "mass") {
  
  # Filter input, use concentrations here, the third factor profile returned
  # by the EPA tool is incorrect and rather odd
  df <- df %>% 
    filter(factor_profile == "concentration_of_species",
           species != !!species_mass)
  
  # Stop if input is incorrect
  if (nrow(df) == 0) {
    stop("Input contains no observations after filtering.", call. = FALSE)
  }
  
  # Calculate the sources' sums, calculate the contribution of each species to
  # the sum, but the mass species is excluded
  df <- df %>% 
    group_by(source) %>% 
    mutate(source_sum = sum(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(contribution = value / source_sum,
           contribution_percent = contribution * 100,
           factor_profile = "species_contributions_to_factor")
  
  return(df)
  
}
