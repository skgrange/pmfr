#' Function to extract PMF factor contributions from the return of
#' \code{\link{read_pmf_source_contributions}}. 
#' 
#' @param df Tibble from \code{\link{read_pmf_source_contributions}}. 
#' 
#' @param model_run Model run extract contributions from.
#' 
#' @param species Which species to extract contributions for. Defaults to mass. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{read_pmf_source_contributions}}, 
#' \code{\link{plot_pmf_mass_factor_contributions}}
#' 
#' @export 
extract_pmf_mass_factor_contributions <- function(df, model_run, species = "mass") {
  
  # Filter table
  df <- df %>% 
    filter(model_run == !!model_run, 
           species == !!species, 
           statistic == "mean")
  
  # Sum masses
  value_sum <- sum(df$value)
  
  # Calculate contributions
  df <- mutate(df, contribution = value / value_sum)
  
  return(df)
  
}
