#' Function to read PMF residuals exported from the EPA PMF tool. 
#' 
#' @param file File to read. 
#' 
#' @param tz Time-zone dates are stored in. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_pmf_residuals <- function(file, tz = "UTC") {
  
  # Read as text
  text <- readr::read_lines(file, progress = FALSE)
  
  # Filter empty rows
  text <- text[text != ""]
  
  # Get indicies
  index_start <- stringr::str_which(text, "Residuals from Base") + 1L
  index_end <- dplyr::lead(index_start)
  index_end <- if_else(is.na(index_end), length(text), index_end)
  index_end <- index_end - 1L
  
  # Split into pieces
  list_data <- purrr::map2(index_start, index_end, ~text[.x:.y])
  
  # Drop training line in first table
  list_data[[1]] <- list_data[[1]][-length(list_data[[1]])]
  
  # Parse text into table and clean a bit
  df <- list_data %>% 
    purrr::map_dfr(readr::read_csv, .id = "table") %>% 
    rename(base_run = Base_Run,
           date = Date_Time) %>% 
    mutate(date = lubridate::mdy_hm(date, tz = tz))
  
  return(df)
  
}
