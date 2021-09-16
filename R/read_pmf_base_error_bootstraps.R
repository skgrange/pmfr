#' Function to read EPA PMF base model bootstrapped errors and statistics. 
#' 
#' @param file File to read. 
#' 
#' @author Stuart K. Grange
#' 
#' @return A named list containing tibbles. 
#' 
#' @export
read_pmf_base_error_bootstraps <- function(file) {
  
  # Read file as text
  text <- readr::read_lines(file, progress = FALSE)
  
  # Get first block, suppression is for missing name warning
  df_mapping <- suppressMessages(
    text[2:(stringr::str_which(text, "^Bootstrapping and Pulling") - 1)] %>% 
      stringr::str_c(collapse = "\n") %>% 
      readr::read_csv(show_col_types = FALSE, progress = FALSE) %>% 
      rename(bootstrap_factor = ...1) %>% 
      purrr::set_names(str_to_underscore) %>% 
      mutate(bootstrap_factor = str_to_underscore(bootstrap_factor)) %>% 
      dplyr::rename_all(~stringr::str_remove(., "^base_"))
  )
  
  # Get start of second table
  index_start <- stringr::str_which(text, "^Columns are")
  
  names_bootstrap <- text[index_start] %>% 
    stringr::str_split_fixed(":", 2) %>% 
    .[, 2] %>% 
    stringr::str_split(", ") %>% 
    .[[1]] %>% 
    stringr::str_trim() %>% 
    str_to_underscore()
  
  df_bootstraps <- text[(index_start + 1):length(text)] %>% 
    stringr::str_c(collapse = "\n") %>% 
    readr::read_csv(col_names = FALSE, show_col_types = FALSE, progress = FALSE)
  
  # Set names
  names(df_bootstraps)[1:length(names_bootstrap)] <- names_bootstrap
  
  # Build return
  list_return <- list(
    mapping = df_mapping,
    bootstraps = df_bootstraps
  )
  
  return(list_return)
  
}


#' Function to tidy EPA PMF's bootstrap mapping tables. 
#' 
#' @param df Bootstrap mapping tibble from 
#' \code{\link{read_pmf_base_error_bootstraps}} or 
#' \code{\link{read_pmf_constrained_error_bootstraps}}. 
#' 
#' @param pairs_only Should the return be filtered only to vaiable pairs? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
tidy_bootstrap_mapping_table <- function(df, pairs_only = FALSE) {
  
  # Reshape table
  df <- df %>% 
    select(-bootstrap_factor,
           -unmapped) %>% 
    tibble::rowid_to_column("x") %>% 
    tidyr::pivot_longer(-x, names_to = "y") %>% 
    mutate(y = stringr::str_remove(y, "factor_"),
           y = as.integer(y))
  
  # Filter to pairs
  if (pairs_only) {
    df <- df %>% 
      filter(x == y) %>% 
      mutate(good = value >= 80)
  }
  
  return(df)
  
}
