#' Print book chapters
#'
#' Prints published book chapters
#'
#' @param x data frame containing information on published book chapters
#'

print_book_chapters <- function(x){
  npap <- nrow(x)
  if(npap){
    cat("### Cap\u00EDtulos de Livros\n")
    for (i in 1:nrow(x)){
      cat(i, ". ",
          x$Authors[[i]],
          ": _", x$Title[i], "._ ",
          "In: ", x$Bookname[i], ", ",
          "vol. ", ifelse(x$Volume == "",
                          "1",
                          x$Volume),
          sep = "")
      if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
        cat(". pp. ", x$Pages[i],
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
