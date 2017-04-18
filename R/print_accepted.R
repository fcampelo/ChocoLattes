#' Print accepted papers
#'
#' Prints accepted papers
#'
#' @param x data frame containing information on accepted papers
#' @param language Language to use in section headers
#'

print_accepted <- function(x,
                           language = c("EN", "PT")){
  npap <- nrow(x)
  if(npap){
    if (language == "PT") cat("### Artigos Aceitos para Publica\u00E7\u00E3o\n")
    if (language == "EN") cat("### Accepted Papers\n")
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
