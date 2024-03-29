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
  
  # Get indices
  index_start <- stringr::str_which(text, "Residuals from Base") + 1L
  index_end <- dplyr::lead(index_start) - 2L
  index_end <- if_else(is.na(index_end), length(text), index_end)
  
  # Split into pieces
  list_data <- purrr::map2(index_start, index_end, ~text[.x:.y])
  
  # Scaled residuals have a trailing row, might need to be read at some point,
  # for now, discard them
  list_data <- purrr::map(list_data, stringr::str_subset, "^,", negate = TRUE)
  
  # Parse text into table and clean a bit
  # Drop Q(true)/Qexp variable too, this needs more thinking at some point
  df <- list_data %>% 
    purrr::map(stringr::str_c, collapse = "\n") %>% 
    purrr::map_dfr(
      readr::read_csv, 
      show_col_types = FALSE, 
      progress = FALSE, 
      .id = "residual_type"
    ) %>% 
    rename(base_run = Base_Run,
           date = Date_Time) %>% 
    select(-dplyr::matches("Qexp")) %>% 
    mutate(residual_type = as.integer(residual_type),
           residual_type = if_else(residual_type %% 2 == 0, "residual_scaled", "residual"), 
           base_run = as.integer(base_run),
           date = lubridate::mdy_hm(date, tz = tz, truncated = 3))
  
  # Rename id if present
  if ("Sample_IDs" %in% names(df)) df <- rename(df, id = Sample_IDs)
  
  return(df)
  
}


#' Function to reshape PMF residuals into tidy data. 
#' 
#' @param df Tibble/data frame from \code{\link{read_pmf_residuals}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
tidy_pmf_residuals <- function(df) {
  
  # Test for id
  if ("id" %in% names(df)) {
    id_variables <- c("residual_type", "base_run", "id", "date")
  } else {
    id_variables <- c("residual_type", "base_run", "date")
  }
  
  # Make longer
  df <- tidyr::pivot_longer(df, -dplyr::all_of(id_variables), names_to = "species")
  
  return(df)
  
}
