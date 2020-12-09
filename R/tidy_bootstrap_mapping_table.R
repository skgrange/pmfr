#' Function to tidy EPA PMF's bootstrap mapping tables. 
#' 
#' @param df Bootstrap mapping tibble from 
#' \code{\link{read_pmf_base_error_bootstraps}} or 
#' \code{\link{read_pmf_constrained_error_bootstraps}}. 
#' 
#' @param pairs_only Should the return be filtered only to vaiable pairs? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
tidy_bootstrap_mapping_table <- function(df, pairs_only = FALSE) {
  
  # Reshape table
  df <- df %>% 
    select(-bootstrap_factor,
           -unmapped) %>% 
    tibble::rowid_to_column("x") %>% 
    tidyr::pivot_longer(-x, names_to = "y") %>% 
    mutate(y = stringr::str_remove(y, "factor_"),
           y = as.integer(y))
  
  # Filter to pairs
  if (pairs_only) {
    df <- filter(df, x == y)
  }
  
  return(df)
  
}
