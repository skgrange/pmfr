#' Function to calculate particulate matter relative contributions. 
#' 
#' @param df Input data frame. 
#' 
#' @param digits Number of decimal points to display in the 
#' \code{contribution_label} variable. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{calculate_missing_mass}}
#' 
#' @examples
#' 
#' # Example input
#' data_example <- tibble::tribble(
#'  ~variable,        ~value,
#'  "mass",           39.7,  
#'  "ec",             6.1,   
#'  "om",             9.2,   
#'  "nitrage",        3.3,   
#'  "ammonium",       1.4,   
#'  "sulphate",       3.3,   
#'  "mineral_dust",   2.7,   
#'  "trace_elements", 6.8,
#'  "missing",        6.9
#'  )
#'  
#' # Determine missing mass
#' calculate_contributions(data_example)
#' 
#' @export
calculate_contributions <- function(df, digits = 1) {
  
  # Check input
  stopifnot(all(c("variable", "value") %in% names(df)))
  
  # Get mass value
  value_mass <- df %>% 
    filter(variable == "mass") %>% 
    pull(value)
  
  # Calculate contribtions and add a few extra things
  df <- df %>% 
    mutate(contribution = value / !!value_mass,
           contribution_percent = contribution * 100,
           contribution_label = round(contribution_percent, digits),
           contribution_label = stringr::str_c(contribution_label, " %"))
  
  return(df)
  
}
