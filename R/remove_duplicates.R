#' Function to remove duplicated entries
#'
#' Cleans up duplicated items on a production data frame based on DOI, ISBN, and Title.
#' This function also removes any rows where the value of "year" is greater than the current year (since these are considered errors)
#'
#' @param x data frame containing information on a certain type of production (journal papers, conference papers, etc.).
#'          Must contain columns "Title" and "Year". Optional (very useful) columns include "DOI" and "ISBN".
#'
#' @return data frame with duplicates removed

remove_duplicates <- function(x){
  if(nrow(x)){
    # Remove duplicates (by DOI) and clean up placeholder values
    if ("DOI" %in% names(x)){
      unique.indx    <- as.numeric(rownames(unique(data.frame(x$DOI)[1])))
      x              <- x[unique.indx, ]
      na.indx        <- grep(pattern = "zNotAvailable", x$DOI)
      x$DOI[na.indx] <- ""
      row.names(x)   <- 1:nrow(x)
    }

    # Remove duplicates (by ISBN) and clean up placeholder values
    if ("ISBN" %in% names(x)){
      unique.indx     <- as.numeric(rownames(unique(data.frame(x$ISBN)[1])))
      x               <- x[unique.indx, ]
      na.indx         <- grep(pattern = "zNotAvailable", x$ISBN)
      x$ISBN[na.indx] <- ""
      row.names(x)    <- 1:nrow(x)
    }

    # Remove duplicates (by Title)
    if ("Title" %in% names(x)){
      titles       <- gsub("\\s", "", tolower(x$Title))
      unique.indx  <- as.numeric(rownames(unique(data.frame(titles)[1])))
      x            <- x[unique.indx, ]
      row.names(x) <- 1:nrow(x)
    }

    # Remove inconsistencies in field "Year" (errors in the .xml due to typos)
    if("Year" %in% names(x)){
      ok.years     <- as.numeric(x$Year) <= as.numeric(format(Sys.time(), "%Y"))
      x            <- x[ok.years, ]
      row.names(x) <- 1:nrow(x)
    }
  }

  return (x)
}
