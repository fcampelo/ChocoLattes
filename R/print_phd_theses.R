#' Print PhD theses
#'
#' Prints PhD theses defended
#'
#' @param x data frame containing information on published phd theses
#' @param language Language to use in section headers
#'

print_phd_theses <- function(x,
                             language = c("EN", "PT")){
  npap <- nrow(x)
  if(npap){
    if (language == "PT") cat("### Teses de Doutorado\n")
    if (language == "EN") cat("### PhD Theses\n")
    for (i in 1:nrow(x)){
      cat(i, ". ",
          x$Author[[i]],
          ": _", x$Title[i], "_ - ", x$Institution[i],
          ". ", x$Advisor[i],
          sep = "")
      if(x$IsMainAdvisor[i]) {
        if (language == "PT") cat(" (orientador principal).\n")
        if (language == "EN") cat(" (main advisor).\n")
      } else {
        if (language == "PT") cat(" (co-orientador).\n")
        if (language == "EN") cat(" (co-advisor).\n")
      }
      cat("\n\n<hr>")
    }
  }
}
