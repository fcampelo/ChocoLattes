#' Capitalize titles
#'
#' Performs capitalization of titles
#'
#' This function attempts to standardize capitalization of titles for the
#' productions listing, using sentence case.
#'
#' @param x dataframe containing fields to be capitalized (using sentence case)
#' @param which.fields character vector with names of dataframe columns to be
#'                     capitalized
#'
#' @return capitalized string
#'
capitalize_titles <- function(x,
                              which.fields   = c("Title",
                                                 "Bookname",
                                                 "Journal",
                                                 "Conference")) {
  for(field in which.fields){
    if (field %in% names(x) && (length(x[, field]) > 0)){
      for (i in 1:length(x[, field])){
        x[i, field] <- paste(toupper(substr(x[i, field], 1, 1)),
                             tolower(substr(x[i, field], 2, nchar(x[i, field]))),
                             sep="")
      }
    }
  }

  return(x)
}
