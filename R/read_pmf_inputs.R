#' Function to read EPA PMF inputs. 
#' 
#' @param file_concentrations File name of concentration file. 
#' 
#' @param file_errors File name of error file. This is optional. 
#' 
#' @param tz Time zone of dates. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#'
#' @export
read_pmf_inputs <- function(file_concentrations, file_errors, tz = "UTC") {
  
  # Load concentrations
  df <- file_concentrations %>%
    readr::read_delim(delim = "\t", col_types = readr::cols(), progress = FALSE) %>%
    mutate(date = lubridate::ymd(date, tz = tz))
  
  # Does the table have an id variable? 
  name_id <- names(which(purrr::map_lgl(df, is.character)))
  
  # Sort out id variables
  if (length(name_id) == 1) {
    name_id <- c("date", name_id)
  } else {
    name_id <- "date"
  }
  
  # Make longer
  df <- tidyr::pivot_longer(df, -dplyr::all_of(name_id), names_to = "variable")
  
  # Load errors
  if (!missing("file_errors")) {
    
    # Load error table
    df_errors <- file_errors %>% 
      readr::read_delim(delim = "\t", col_types = readr::cols(), progress = FALSE) %>% 
      mutate(date = lubridate::ymd(date, tz = tz)) %>% 
      tidyr::pivot_longer(
        -dplyr::all_of(name_id), names_to = "variable", values_to = "error"
      )
    
    # Join
    df <- left_join(df, df_errors, by = c(name_id, "variable"))
    
  }
  
  return(df)
  
}
