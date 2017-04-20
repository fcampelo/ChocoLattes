#' Print MSc theses
#'
#' Prints MSc theses defended
#'
#' @param x data frame containing information on published MSc theses
#' @param language Language to use in section headers
#'

print_msc_theses <- function(x,
                             language = c("EN", "PT")){
  npap <- nrow(x)
  if(npap){
    if (language == "PT") cat("### Disserta\u00E7\u00F5es de Mestrado\n")
    if (language == "EN") cat("### MSc Dissertations\n")
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
