#' Print books
#'
#' Prints published books
#'
#' @param x data frame containing information on published books
#'

print_books <- function(x){
  npap <- nrow(x)
  if(npap){
    cat("### Livros Publicados\n")
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
        cat(x$Pages[i], " pages. ",
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
