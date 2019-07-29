#' Function to export a data table for use with the EPA PMF tool. 
#' 
#' @param df Data frame to export. 
#' 
#' @param file File name to export data to. 
#' 
#' @param zero_check Should the table be checked for values less than and equal 
#' to 0? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible tibble. 
#' 
#' @export
export_pmf_input_data <- function(df, file, zero_check = TRUE) {
  
  # Checks
  # No type check for date
  if (names(df)[1] != "date") {
    stop("First variable of the input must be called `date`...", call. = FALSE)
  }
  
  if (anyNA(df)) {
    stop("Missing values (`NA`) are not allowed...", call. = FALSE)
  }
  
  types <- df[-1] %>% 
    purrr::map_chr(class) %>% 
    unique()
  
  if (!all(types %in% c("numeric", "integer"))) {
    stop("All columns apart from `date` must be numeric...", call. = FALSE)
  }
  
  if (zero_check) {
    if (any(df[, -1] <= 0)) {
      stop("Values which are 0 or negative are not allowed...", call. = FALSE)
    } 
  }
  
  # Export file
  readr::write_delim(df, file, delim = "\t")
  
  return(invisible(df))
  
}
