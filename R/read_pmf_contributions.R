#' Function to read PMF source contributions exported from the EPA PMF tool. 
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
read_pmf_contributions <- function(file, tz = "UTC") {
  
  # Load data
  # Supression is for two missing column names
  suppressWarnings(
    df <- readr::read_csv(
      file, 
      skip = 3, 
      col_types = readr::cols(), 
      progress = FALSE
    )
  )
  
  # Clean names
  names(df)[1:2] <- c("model_run", "date")
  names(df)[-1:-2] <- stringr::str_to_lower(names(df)[-1:-2])
  names(df)[-1:-2] <- stringr::str_replace_all(names(df)[-1:-2], " ", "_")
  
  # Parse dates
  df <- df %>% 
    mutate(model_run = as.integer(model_run),
           date = lubridate::mdy_hm(date, tz = tz))
  
  return(df)
  
}


#' Function to reshape PMF contributions into tidy data. 
#' 
#' @param df Tibble/data frame from \code{\link{read_pmf_contributions}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
tidy_pmf_contributions <- function(df) {
  tidyr::gather(df, factor, value, -c(model_run, date))
}
