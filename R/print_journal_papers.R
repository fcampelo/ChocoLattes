#' Print journal papers
#'
#' Prints published journal papers
#'
#' @param x data frame containing information on published papers
#'

print_journal_papers <- function(x){
  npap <- nrow(x)
  if(npap){
    cat("### Artigos em Peri\u00F3dicos\n")
    for (i in 1:nrow(x)){
      cat(i, ". ",
          x$Authors[[i]],
          ": _", x$Title[i], "._ ",
          x$Journal[i], " ",
          x$Volume[i],
          "(", x$Issue[i], ")",
          sep = "")
      if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
        cat(":", x$Pages[i],
            sep = "")
      }
      cat(", ", x$Year[i],
          sep = "")
      if(!is.null(x$DOI[i]) && !is.na(x$DOI[i]) && x$DOI[i] != ""){
        cat(".<br/>[[DOI: ",
            x$DOI[i],
            "](http://dx.doi.org/",
            x$DOI[i], ")]",
            sep = "")
      }
      cat("\n\n<hr>",
          sep = "")
    }
  }
}
