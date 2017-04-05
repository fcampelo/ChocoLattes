#' Capitalize author names
#'
#' Performs capitalization of author names
#'
#' This function attempts to standardize capitalization of author names for the production listings
#'
#' @param x data frame containing a column "Authors", author names (separated by commas, if multiple)
#' @param make.lower character vector containing name conectors to keep in lowercase
#'
#' @return data frame with (hopefully) correctly capitalized author names in column "Authors".

capitalize_authors <- function(x, make.lower = c("De", "Da", "Do", "E")) {
  if("Authors" %in% names(x) && nrow(x)){
    capfun <- function(y){
      s <- strsplit(y, " ")[[1]]
      s <- paste(toupper(substring(s, 1, 1)),
                 tolower(substring(s, 2)),
                 sep = "", collapse = " ")
      if(length(keep.lower)){
        for (i in seq_along(keep.lower)){
          key <- paste0(" ", keep.lower[i], " ")
          s <- gsub(pattern     = key,
                    replacement = tolower(key),
                    x           = s)
        }
      }
      s <- gsub(pattern     = "(\\.{0}[a-z]\\.)",
                replacement = "\\U\\1",
                x           = s,
                perl        = TRUE)
      return(s)
    }
    x$Authors <- lapply(x$Authors, FUN = capfun)
  }
  return(x)
}
