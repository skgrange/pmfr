#' Function to export a data table for use with the EPA PMF tool. 
#' 
#' @param df Data frame to export. 
#' 
#' @param file File name to export data to. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible tibble. 
#' 
#' @export
export_pmf_input_data <- function(df, file) {
  
  # Checks
  # Missing-ness
  # Dates
  
  readr::write_delim(df, delim = "\t")
  return(invisible(df))
  
}
