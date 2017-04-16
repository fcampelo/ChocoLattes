#' Extract author names
#'
#' Extracts author names from items
#'
#' This function extracts author names from bibliographic items.
#'
#' @param item a single bibliographic item, selected from a lattes list object.
#'
#' @return character string containing author names in "name surname" format,
#'     separated by commas.

get_authors <- function(item){
  indx     <- which(names(item) == "AUTORES")
  nauthors <- length(indx)
  Authors  <- make_name_surname(item[[indx[1]]]$`NOME-COMPLETO-DO-AUTOR`)
  if (nauthors > 1){
    for (j in 2:nauthors){
      Authors <- paste0(Authors,
                        ", ",
                        make_name_surname(item[[indx[j]]]$`NOME-COMPLETO-DO-AUTOR`))
    }
  }
  return(Authors)
}
