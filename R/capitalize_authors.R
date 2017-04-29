#' Capitalize author names
#'
#' Performs capitalization of author names
#'
#' This function attempts to standardize capitalization of author names for the production listings
#'
#' @param x data frame containing a column "Authors" (or "Author"),
#' which should describe author names (separated by commas, if multiple) for a given
#' production type.
#' @param make.lower character vector containing name conectors to keep in lowercase
#' @param author.aliases list vector with author aliases.
#' See [lattes_to_list()] for details.
#'
#' @return data frame with (hopefully) correctly capitalized author names in column "Authors".
capitalize_authors <- function(x,
                               make.lower = c('De', 'Di', 'Do', 'Da',
                                              'Dos', 'Das', 'Dello', 'Della',
                                              'Dalla', 'Dal', 'Del', 'E',
                                              'Em', 'Na', 'No', 'Nas',
                                              'Nos', 'Van', 'Von',
                                              'Y'),
                               author.aliases = NULL) {
  if("Authors" %in% names(x) && nrow(x)){
    x$Authors <- lapply(x$Authors, FUN = capfun, make.lower = make.lower)
  }
  if("Author" %in% names(x) && nrow(x)){
    x$Author <- lapply(x$Author, FUN = capfun, make.lower = make.lower)
  }

  if(!is.null(author.aliases)){
    if(!is.list(author.aliases)) stop("author.aliases must be a list object")
    for (i in seq_along(author.aliases)){
      if(!is.character(author.aliases[[i]])) stop("Each element of author.aliases must be a character vector")
      if (length(author.aliases[[i]]) > 1){
        for (j in 2:length(author.aliases[[i]])){
          if("Authors" %in% names(x) && nrow(x)){
            x$Authors <- gsub(pattern     = author.aliases[[i]][j],
                              replacement = author.aliases[[i]][1],
                              x           = x$Authors,
                              ignore.case = TRUE)
          }
          if("Author" %in% names(x) && nrow(x)){
            x$Author <- gsub(pattern     = author.aliases[[i]][j],
                             replacement = author.aliases[[i]][1],
                             x           = x$Author,
                             ignore.case = TRUE)
          }
        }
      }
    }
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
