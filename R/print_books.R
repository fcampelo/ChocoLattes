#' Print books
#'
#' Prints published books
#'
#' @param x data frame containing information on published books
#' @param language Language to use in section headers
#'

print_books <- function(x,
                        language = c("EN", "PT")){
  npap <- nrow(x)
  if(npap){
    if (language == "PT") cat("### Livros Publicados\n")
    if (language == "EN") cat("### Books\n")
    for (i in 1:nrow(x)){
      cat(i, ". ",
          x$Authors[[i]],
          ": **", x$Bookname[i], "**. ",
          sep = "")
      if(x$Publisher[i] != ""){
        cat(x$Publisher[i], ", ",
            sep = "")
      }
      if(x$City[i] != ""){
        cat(x$City[i], ". ",
            sep = "")
      }
      if(x$Pages[i] != ""){
        cat(x$Pages[i], " pgs. ",
            sep = "")
      }
      cat(x$Year, ". ",
          sep = "")
      if(x$ISBN[i] != ""){
        cat("ISBN: ", x$ISBN[i], ".",
            sep = "")
      }
      cat("\n\n<hr>",
          sep = "")
    }
  }
}
