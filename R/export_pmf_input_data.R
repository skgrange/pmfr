#' Function to export a data table for use with the EPA PMF tool. 
#' 
#' @param df Data frame to export. 
#' 
#' @param file File name to export data to. 
#' 
#' @param date_check Should dates be checked? Dates are not strictly required 
#' for the EPA PMF tool so this checking logic can be disabled if desired. 
#' 
#' @param format_date Should the dates be formatted? 
#' 
#' @param zero_check Should the table be checked for values less than and equal 
#' to 0? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible tibble. 
#' 
#' @export
export_pmf_input_data <- function(df, file, date_check = TRUE, format_date = TRUE, 
                                  zero_check = TRUE) {
  
  # Checks
  if (anyNA(df)) {
    stop("Missing values (`NA`) are not allowed...", call. = FALSE)
  }
  
  if (date_check) {
    # No type check for date yet
    if (names(df)[1] != "date") {
      stop("First variable of the input must be called `date`...", call. = FALSE)
    }
    
    if (anyDuplicated(df$date) != 0) {
      stop("Duplicated dates are not allowed...", call. = FALSE)
    }
    
    types <- df[-1] %>% 
      purrr::map_chr(class) %>% 
      unique()
    
  } else {
    types <- df %>% 
      purrr::map_chr(class) %>% 
      unique()
  }
  
  if (!all(types %in% c("numeric", "integer"))) {
    stop("All columns apart from `date` must be numeric...", call. = FALSE)
  }
  
  if (zero_check) {
    if (any(df[, -1] <= 0)) {
      stop("Values which are 0 or negative are not allowed...", call. = FALSE)
    } 
  }
  
  # Format date
  if (date_check && format_date) {
    if (all(lubridate::hour(df$date) == 0)) {
      df$date <- format(df$date, format = "%Y-%m-%d")
    } else {
      df$date <- format(df$date, format = "%Y-%m-%d %H:%M:%S")
    }
  }

  # Export tab delimited file
  readr::write_delim(df, file, delim = "\t")
  
  return(invisible(df))
  
}
