#' Function to read EPA PMF inputs. 
#' 
#' @param file_concentrations File name of concentration file. 
#' 
#' @param file_errors File name of error file. 
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
  df_concentrations <- file_concentrations %>%
    readr::read_delim(delim = "\t", col_types = readr::cols()) %>%
    mutate(date = lubridate::ymd(date, tz = tz)) %>% 
    tidyr::pivot_longer(-date, names_to = "variable")
  
  # Load errors
  df_errors <- file_errors %>% 
    readr::read_delim(delim = "\t", col_types = readr::cols()) %>% 
    mutate(date = lubridate::ymd(date, tz = tz)) %>% 
    tidyr::pivot_longer(-date, names_to = "variable", values_to = "error")
  
  # Join
  df <- left_join(df_concentrations, df_errors, by = c("date", "variable"))
  
  return(df)
  
}
