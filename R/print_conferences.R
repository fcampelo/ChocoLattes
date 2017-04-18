#' Print conference papers
#'
#' Prints published conference papers
#'
#' @param x data frame containing information on published conference papers
#' @param isIntl logical flag, TRUE for international conferences, FALSE for
#' national/regional
#' @param language Language to use in section headers
#'

print_conferences <- function(x,
                              isIntl   = TRUE,
                              language = c("EN", "PT")){
  x <- x[which(x$Internac == isIntl), ]
  npap <- nrow(x)
  if(npap){
    if (language == "PT"){
      cat("### Artigos e Resumos em Confer\u00EAncias",
        ifelse(isIntl,
               "Internacionais\n",
               "Nacionais e Regionais\n"))
    }
    if (language == "EN"){
      cat("### Works in Conference Proceedings ",
          ifelse(isIntl,
                 "(International)\n",
                 "(National / Regional)\n"))
    }
    for (i in 1:nrow(x)){
      cat(i, ". ",
          x$Authors[[i]],
          ": _", x$Title[i], "._ ",
          x$Conference[i], ", ",
          sep = "")
      if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
        cat("pp. ", x$Pages[i],
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
