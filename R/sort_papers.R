#' Sort publications by year
#'
#' Sorts publications by year
#'
#' @param x data frame containing information on a certain type of production (journal papers, conference papers, etc.).
#'          Must contain column "Year". Optional (useful) columns include "DOI" and "ISBN".
#' @param decreasing flag, sort in decreasing or increasing order
#'
#' @return ordered data frame

sort_papers <- function(x, decreasing = TRUE){
  if(nrow(x)){
    if ("DOI" %in% names(x)){
      x <- x[order(x$DOI), ]
    }
    if ("ISBN" %in% names(x)){
      x <- x[order(x$ISBN), ]
    }

    x <- x[order(x$Year, decreasing = decreasing), ]
    row.names(x) <- as.character(1:nrow(x))
  }
  return(x)
}
