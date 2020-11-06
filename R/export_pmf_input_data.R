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
#' @param id_variable Variable name in \code{df} which contains an identifying
#' variable. This is optional. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible \code{df}.  
#' 
#' @export
export_pmf_input_data <- function(df, file, date_check = TRUE, format_date = TRUE, 
                                  zero_check = TRUE, id_variable = NA) {
  
  # Location of date variable
  index_date <- which(names(df) == "date")
  
  # Location of id variable
  index_id <- which(names(df) == id_variable)
  
  # Combine identifier indices 
  if (length(index_id) == 0) { 
    index_id <- index_date
  } else {
    index_id <- c(index_date, index_id)
  }
  
  # Input checks
  if (date_check) {
    
    if (length(index_date) == 0) {
      stop("Input table must contain a variable named `date`.", call. = FALSE)
    }
    
    # Data type check
    if (!lubridate::is.POSIXt(df$date)) {
      stop("`date` must be of a POSIXt data type.", call. = FALSE)
    }
    
  }
  
  # Missing values check
  if (anyNA(df)) {
    stop("Missing values (`NA`) are not allowed.", call. = FALSE)
  }
  
  # Select non identifier variables
  df_no_ids <- select(df, -!!index_id)
  
  # Check data types
  types <- df_no_ids %>% 
    purrr::map_chr(class) %>% 
    unique()
  
  if (!all(types %in% c("numeric", "integer"))) {
    stop(
      "All columns apart from `date` and `id_variable` must be numeric.", 
      call. = FALSE
    )
  }
  
  if (zero_check) {
    if (any(df_no_ids <= 0)) {
      stop("Values which are 0 or negative are not allowed.", call. = FALSE)
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
