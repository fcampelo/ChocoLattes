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
    x$Authors <- lapply(x$Authors, FUN = capfun, make.lower = make.lower)
  }
  if("Author" %in% names(x) && nrow(x)){
    x$Author <- lapply(x$Author, FUN = capfun, make.lower = make.lower)
  }
  return(x)
}

#=======
capfun <- function(y, make.lower){
  s <- strsplit(y, " ")[[1]]
  s <- paste(toupper(substring(s, 1, 1)),
             tolower(substring(s, 2)),
             sep = "", collapse = " ")
  if(length(make.lower)){
    for (i in seq_along(make.lower)){
      key <- paste0(" ", make.lower[i], " ")
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
