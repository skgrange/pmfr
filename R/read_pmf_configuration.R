#' Function to read EPA PMF configuration file. 
#' 
#' @param file File to read. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_pmf_configuration <- function(file) {
  
  # Not using an xml parser here, just string process
  text <- readr::read_lines(file)
  
  matrix_text <- stringr::str_subset(text, "key=") %>% 
    stringr::str_split_fixed("key=|value=", 3)
  
  key <- matrix_text[, 2] %>% 
    stringr::str_remove_all('"|/>') %>% 
    stringr::str_trim()
  
  value <- matrix_text[, 3] %>% 
    stringr::str_remove_all('"|/>') %>% 
    stringr::str_trim() %>% 
    stringr::str_replace_all("\\\\", "/")
  
  value <- if_else(value == "", NA_character_, value)
  
  # To tibble
  df <- tibble(file, key, value)
  
  return(df)
  
}
