#' Print journal papers
#'
#' Prints published journal papers
#'
#' @param x data frame containing information on published papers
#' @param language Language to use in section headers
#'

print_journal_papers <- function(x,
                                 language = c("EN", "PT")){
  npap <- nrow(x)
  if(npap){
    if (language == "PT") cat("### Artigos em Peri\u00F3dicos\n")
    if (language == "EN") cat("### Journal Papers\n")
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
