#' Function to read PMF diagnostics exported from the EPA PMF tool. 
#' 
#' @param file File to read. 
#' 
#' @param tz Time zone the dates are stored in. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list containing tibbles and an integer. 
#' 
#' @export
read_pmf_diagnostics <- function(file, tz = "UTC") {
  
  # Read file as text
  text <- readr::read_lines(file, progress = FALSE)
  
  # Build list
  list_components <- list(
    analysis_summary = format_analysis_summary(text),
    input_data_statistics = format_input_data_statistics(text),
    base_run_summary = format_base_run_summary(text),
    base_run_summary_table = format_base_run_summary_table(text),
    scaled_residual_analysis = format_scaled_residual_analysis(text),
    # scaled_residual_analysis_sum_d = format_scaled_residual_analysis_sum_d(text), # This needs another look
    regression_diagnostics = format_ks_test(text),
    scaled_outlier_residuals_by_species = format_scaled_residuals_by_species(text, tz = tz),
    scaled_outlier_residuals_by_date = format_scaled_residuals_by_date(text, tz = tz)
  )
  
  # Find and best model run
  model_run_best <- list_components %>% 
    purrr::pluck("base_run_summary_table") %>% 
    filter(converged) %>% 
    arrange(q_robust) %>% 
    slice(1) %>% 
    pull(model_run)
  
  # Add extra piece
  list_components <- c(list_components, model_run_best = model_run_best)
  
  return(list_components)
  
}



format_analysis_summary <- function(text) {
  
  # To-do: generalise with the f-peak functions
  
  index_start <- stringr::str_which(text, "Analysis Summary") + 1
  index_end <- stringr::str_which(text, "Input Data Statistics") - 1
  
  # Split
  matrix_text <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    stringr::str_split_fixed(":", 2) 
  
  matrix_text[, 1] <- matrix_text[, 1] %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" ", "_")
  
  matrix_text[, 2] <- matrix_text[, 2] %>% 
    stringr::str_trim() %>% 
    stringr::str_remove("^,")
  
  # Make a nicely formatted tibble
  df <- matrix_text[, 2] %>% 
    purrr::set_names(matrix_text[, 1]) %>% 
    tibble::enframe() %>%
    tidyr::pivot_wider(names_from = "name") %>% 
    dplyr::mutate_all(~if_else(. == "", NA_character_, .)) %>% 
    mutate(time_of_run = lubridate::mdy_hm(time_of_run, tz = "UTC"),
           concentration_file = stringr::str_replace_all(concentration_file, "\\\\", "/"),
           configuration_file = stringr::str_replace_all(configuration_file, "\\\\", "/"),
           uncertainty_file = stringr::str_replace_all(uncertainty_file, "\\\\", "/"))
  
  return(df)
  
}


format_input_data_statistics <- function(text) {
  
  index_start <- stringr::str_which(text, "Input Data Statistics") + 1
  index_end <- stringr::str_which(text, "Excluded Samples") - 1
  
  df <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    stringr::str_c(collapse = "\n") %>% 
    readr::read_csv(show_col_types = FALSE, progress = FALSE) %>% 
    purrr::set_names(
      c(
        "species", "category", "signal_to_noise", "minimum", "lower_quartile",
        "median", "upper_quartile", "maximum", "percent_modelled_samples", 
        "percent_raw_samples"
      )
    ) %>% 
    mutate(percent_modelled_samples = stringr::str_remove(percent_modelled_samples, "%"),
           percent_raw_samples = stringr::str_remove(percent_raw_samples, "%"),
           percent_modelled_samples = as.numeric(percent_modelled_samples),
           percent_raw_samples = as.numeric(percent_raw_samples),
           category = stringr::str_to_lower(category))
  
  return(df)
  
}


format_base_run_summary <- function(text) {
  
  index_start <- stringr::str_which(text, "Base Run Summary") + 1
  index_end <- stringr::str_which(text, "Base run summary table") - 1
  
  matrix_text <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    stringr::str_split_fixed(":", 2) 
  
  matrix_text[, 1] <- matrix_text[, 1] %>% 
    stringr::str_replace_all("\\s*\\([^\\)]+\\)", "") %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" |-", "_") %>% 
    stringr::str_remove("_$")
  
  matrix_text[, 2] <- matrix_text[, 2] %>% 
    stringr::str_trim() %>% 
    stringr::str_remove("^,")
  
  df <- matrix_text[, 2] %>% 
    purrr::set_names(matrix_text[, 1]) %>% 
    tibble::enframe() %>% 
    tidyr::spread(name, value, convert = TRUE)
  
  return(df)
  
}


format_base_run_summary_table <- function(text) {
  
  index_start <- stringr::str_which(text, "Base run summary table") + 1
  index_end <- stringr::str_which(text, "Scaled residual analysis") - 1
  
  df <- text[index_start:index_end] %>% 
    .[. != ""] %>% 
    stringr::str_c(collapse = "\n") %>% 
    readr::read_csv(show_col_types = FALSE, progress = FALSE) %>% 
    purrr::set_names(
      c(
        "model_run", "q_robust", "q_true", "converged", "number_of_steps", 
        "q_true_q_exp"
        )
    ) %>% 
    mutate(model_run = as.integer(model_run),
           converged = if_else(converged == "Yes", TRUE, FALSE),
           q_true_robust_ratio = q_true / q_robust)
  
  return(df)
  
}


format_ks_test <- function(text) {
  
  index_start <- stringr::str_which(text, "Regression diagnostics") + 1
  index_end <- stringr::str_which(text, "dates by species|IDs by species") - 1
  
  # Filter to the table pieces, then read as a data frame
  df <- purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
    purrr::map(stringr::str_c, collapse = "\n") %>% 
    purrr::map_dfr(
      readr::read_csv, 
      skip = 1, 
      progress = FALSE, 
      show_col_types = FALSE, 
      .id = "model_run"
    ) %>% 
    mutate(model_run = as.integer(model_run)) %>% 
    purrr::set_names(
      c(
        "model_run", "species", "intercept", "slope", "standard_error", 
        "r_squared", "statistic", "p_value"
      )
    )
  
  return(df)
  
}


format_scaled_residual_analysis <- function(text) {
  
  index_start <- stringr::str_which(text, "Scaled residual analysis") + 1
  index_end <- stringr::str_which(text, "Sum of d across") - 1
  
  # Message and warning suppression is for an empty column and a raised problem
  suppressWarnings(
    suppressMessages(
      df <- text[index_start:index_end] %>% 
        .[. != ""] %>% 
        stringr::str_c(collapse = "\n") %>% 
        readr::read_csv(skip = 1, show_col_types = FALSE, progress = FALSE) %>% 
        dplyr::select_if(Negate(is.logical))
    )
  )
  
  names(df)[1] <- "model_run"
  
  return(df)
  
}


# format_scaled_residual_analysis_sum_d <- function(text) {
#   
#   index_start <- stringr::str_which(text, "Sum of d across") + 1
#   index_end <- stringr::str_which(text, "Analysis of Base Run") - 1
#   
#   df <- text[index_start:index_end] %>% 
#     .[. != ""] %>% 
#     stringr::str_c(collapse = "\n") %>% 
#     readr::read_csv(show_col_types = FALSE, progress = FALSE)
#   
#   # First column is bad and needs cleaning
#   names(df)[1] <- "model_run"
#   
#   df_ <- df %>% 
#     mutate(model_run = stringr::str_squish(model_run)) %>% 
#     tidyr::separate(model_run, into = c("model_run", "model_run_two"), sep = " ")
#   
#   
#   # # Message and warning suppression is for an empty column and a raised problem
#   # suppressWarnings(
#   #   suppressMessages(
#   #     df <- text[index_start:index_end] %>% 
#   #       .[. != ""] %>% 
#   #       stringr::str_c(collapse = "\n") %>% 
#   #       readr::read_csv(show_col_types = FALSE, progress = FALSE) %>% 
#   #       dplyr::select_if(Negate(is.logical))
#   #   )
#   # )
#   
#   names(df)[1:2] <- c("model_run", "model_run_two")
#   
#   return(df)
#   
# }


format_scaled_residuals_by_species <- function(text, tz) {
 
  index_start <- stringr::str_which(text, "dates by species|IDs by species") + 1
  index_end <- stringr::str_which(text, "species by date|species by ID") - 1
  
  # Filter to the table pieces, then read as a tibble
  df <- purrr::map2(index_start, index_end, ~text[.x:.y]) %>% 
    purrr::map(stringr::str_c, collapse = "\n") %>% 
    purrr::map_dfr(
      readr::read_csv, show_col_types = FALSE, progress = FALSE, .id = "model_run"
    )
  
  # When the run has an id variable
  if ("IDs" %in% names(df)) {
    names <- c("model_run", "species", "id", "date", "residual")
  } else {
    names <- c("model_run", "species", "date", "residual")
  }
  
  # Set names and sort out data types
  df <- df %>% 
    purrr::set_names(names) %>% 
    mutate(model_run = as.integer(model_run),
           date = lubridate::mdy_hm(date, tz = tz, truncated = 3))
  
  return(df)
  
}


format_scaled_residuals_by_date <- function(text, tz) {
  
  # Get indices
  index_start <- stringr::str_which(text, "species by date") + 1
  index_end <- stringr::str_which(text, "\\*\\*\\*\\* Analysis of") - 1
  
  # This table is not present when an id is used
  if (length(index_start) == 0) return(tibble())
  
  # Drop first element which is the header for the section and the final element
  # is the end of the last table
  index_end <- c(index_end[-1], length(text))
  
  # Isolate data
  df <- purrr::map2(index_start, index_end, ~text[.x:.y])
  
  # Logic is for when there are only headers
  if (any(purrr::map_int(df, length) == 1)) {
    return(tibble())
  } else {
    df <- df %>% 
      purrr::map(stringr::str_c, collapse = "\n") %>% 
      purrr::map_dfr(
        readr::read_csv, 
        show_col_types = FALSE,
        progress = FALSE,
        .id = "model_run"
      ) %>% 
      purrr::set_names(c("model_run", "date", "species", "residual")) %>% 
      mutate(model_run = as.integer(model_run),
             date = lubridate::mdy_hm(date, tz = tz, truncated = 3))
  }
  
  return(df)
  
}
