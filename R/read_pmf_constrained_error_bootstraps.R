#' Function to read EPA PMF constrained model bootstrapped errors and statistics. 
#' 
#' @param file File to read. 
#' 
#' @author Stuart K. Grange
#' 
#' @return A named list containing tibbles. 
#' 
#' @export
read_pmf_constrained_error_bootstraps <- function(file) {
  
  # Return empty list if no file is passed
  if (length(file) == 0) return(list())
  
  # Read file as text
  text <- readr::read_lines(file, progress = FALSE)
  
  # Format the two units in the file
  list_data <- list(
    mapping = format_constrained_bootstrap_mapping(text),
    bootstraps = format_constrained_bootstrap_bootstraps(text)
  )
  
  return(list_data)
  
}


format_constrained_bootstrap_mapping <- function(text) {
  
  # Where does the unit start and finish? 
  index_start <- stringr::str_which(text, "Mapping of Constrained") + 2L
  index_end <- stringr::str_which(text, "Constrained Bootstrapping") - 3L
  
  # Parse table
  # Message suppression is for missing first name
  df <- suppressMessages(
    text[index_start:index_end] %>% 
      stringr::str_c(collapse = "\n") %>% 
      readr::read_csv(show_col_types = FALSE, progress = FALSE)
  )
  
  # Clean names
  variable_names <- names(df)
  variable_names[1] <- "bootstrap_factor"
  variable_names <- str_to_underscore(variable_names)
  variable_names <- stringr::str_remove(variable_names, "^base_")
  
  # Clean variables and data types
  df <- df %>% 
    purrr::set_names(variable_names) %>% 
    mutate(bootstrap_factor = str_to_underscore(bootstrap_factor),
           across(tidyselect::vars_select_helpers$where(is.numeric), as.integer))
  
  return(df)
  
}


format_constrained_bootstrap_bootstraps <- function(text) {
  
  # Where does the unit start and finish? 
  index_start <- stringr::str_which(text, "There is a block") + 1L
  index_end <- length(text)
  
  # Parse table
  df <- text[(index_start + 2L):index_end] %>% 
    stringr::str_c(collapse = "\n") %>% 
    readr::read_csv(col_names = FALSE, show_col_types = FALSE, progress = FALSE)
  
  # Get and clean names
  names_bootstrap <- text[index_start] %>% 
    stringr::str_split_fixed(":", 2) %>% 
    .[, 2] %>% 
    stringr::str_split(", ") %>% 
    .[[1]] %>% 
    stringr::str_trim() %>% 
    str_to_underscore()
  
  # Set names
  names(df)[1:length(names_bootstrap)] <- names_bootstrap
  
  return(df)
  
}
