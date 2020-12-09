# Functions pulled from threadr
str_to_underscore <- function(x) {
  
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub(" ", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_trim(x)
  return(x)
  
}


str_rm_round_brackets <- function(x) {
  stringr::str_replace_all(x, "\\s*\\([^\\)]+\\)", "")
}


na_locf <- function(x, na.rm = FALSE) zoo::na.locf(x, na.rm = na.rm)
