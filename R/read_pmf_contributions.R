#' Function to read PMF source contributions exported from the EPA PMF tool. 
#' 
#' @param file File to read. 
#' 
#' @param tz Time-zone which the dates are stored in. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_pmf_contributions <- function(file, tz = "UTC") {
  
  # Load data
  # Suppression is for missing column names
  suppressWarnings(
    df <- readr::read_csv(
      file, 
      skip = 3, 
      col_types = readr::cols(), 
      progress = FALSE
    )
  )
  
  # Where do the concentration tables start? 
  index_concentration_start <- stringr::str_which(df$X1, "conc. units")[1]
  
  # Raise message if no concentration data, this is key for analysis
  if (is.na(index_concentration_start)) {
    message("`Total mass` variable was not used in the EPA PMF tool, only normalised concentrations are available...")
  }
  
  # If missing, all data will be normalised
  index_concentration_start <- if_else(
    is.na(index_concentration_start), nrow(df) + 1L, index_concentration_start
  )
  
  # Does this file have an id variable?
  has_id <- stringr::str_detect(names(df), "^X")[3]
  
  # Clean names
  if (has_id) {
    names(df)[1:3] <- c("model_run", "id", "date")
    names(df)[-1:-3] <- stringr::str_to_lower(names(df)[-1:-3])
    names(df)[-1:-3] <- stringr::str_replace_all(names(df)[-1:-3], " ", "_")
  } else {
    names(df)[1:2] <- c("model_run", "date")
    names(df)[-1:-2] <- stringr::str_to_lower(names(df)[-1:-2])
    names(df)[-1:-2] <- stringr::str_replace_all(names(df)[-1:-2], " ", "_")
  }
  
  # Give id variable
  df <- df %>% 
    tibble::rowid_to_column() %>% 
    mutate(
      unit = if_else(rowid < !!index_concentration_start, "normalised", "concentrations")
    )
  
  # If has id, this variable is missing in the concentration table so needs to
  # be added
  if (has_id) {
    
    # Create vector of new names
    names_concentrations <- stringr::str_subset(names(df), "^id|unit", negate = TRUE)
    
    # Get the id vector
    id_vector <- df %>% 
      filter(unit == "normalised") %>% 
      pull(id)
    
    # Select the table, drop the trailing variables, and add the id vector from
    # the normalised unit
    df_concentrations <- df %>% 
      filter(unit == "concentrations",
             !is.na(date)) %>% 
      select(-tail(names(.), 2)) %>% 
      purrr::set_names(names_concentrations) %>% 
      mutate(across(everything(), type.convert, as.is = TRUE),
             id = !!id_vector,
             unit = "concentrations")
    
    # Bind the two tables
    df <- df %>% 
      filter(unit != "concentrations") %>% 
      mutate(across(everything(), type.convert, as.is = TRUE)) %>% 
      bind_rows(df_concentrations)
    
  } else {
    
    # For when there are is not an id variable
    df <- df %>% 
      filter(!stringr::str_detect(model_run, "Factor Contributions"),
             !is.na(date)) %>% 
      mutate(across(everything(), type.convert, as.is = TRUE))
    
  }

  # Parse dates and tidy up a bit
  df <- df %>% 
    mutate(
      date = lubridate::mdy_hm(date, tz = tz, truncated = 3),
      across(dplyr::starts_with("factor_"), ~if_else(. == -999, NA_real_, .)),
      model_type = "base"
    ) %>% 
    select(-rowid) %>% 
    relocate(model_type,
             model_run,
             unit,
             date)
  
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
  
  # Test for id
  if ("id" %in% names(df)) {
    id_variables <- c("model_run", "unit", "date", "id")
  } else {
    id_variables <- c("model_run", "unit", "date")
  }
  
  # Test for model type too
  if ("model_type" %in% names(df)) {
    id_variables <- c("model_type", id_variables)
  }
  
  # Make longer and add the sum and contributions
  df <- df %>% 
    tidyr::pivot_longer(-dplyr::all_of(id_variables), names_to = "factor") %>% 
    group_by(model_run, 
             unit,
             date) %>% 
    mutate(value_sum = sum(value, na.rm = TRUE),
           contribution = value / value_sum) %>% 
    ungroup()
  
  return(df)
  
}
