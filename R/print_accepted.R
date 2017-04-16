#' Print accepted papers
#'
#' Prints accepted papers
#'
#' @param x data frame containing information on accepted papers
#'

print_accepted <- function(x){
  npap <- nrow(x)
  if(npap){
    cat("### Artigos Aceitos para Publica\u00E7\u00E3o\n")
    for (i in 1:npap){
      cat(i, ". ",
          x$Authors[[i]], ": _",
          x$Title[i], "._ ",
          x$Journal[i], ", ",
          x$Year[i],
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
