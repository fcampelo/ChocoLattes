#' Put author names in "Name Surname" format
#'
#' Put author names in "Name Surname" format
#'
#' @param x string containing author name, either in "Surname, Name" or "Name Surname" format
#'
#' @return string containing author name in "Name Surname" format

make_name_surname <- function(x){
  s <- strsplit(x, ",")[[1]]
  if (length(s) == 2) s <- paste(s[2], s[1])
  return(sub("^\\s+", "", s))
}
