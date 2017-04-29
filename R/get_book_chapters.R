#' Extract published book chapters
#'
#' Extracts published book chapters from Lattes list.
#'
#' This function extracts relevant information on published book chapters from a Lattes list.
#'
#' @param x Lattes list (generated internally in [lattes_to_list()])
#' @param ID a unique identifier for each CV being processed.
#'
#' @return data frame containing parsed information on published book chapters

get_book_chapters <- function(x,
                              ID = stats::runif(1)){
  n.items <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`CAPITULOS-DE-LIVROS-PUBLICADOS`)
  out.df  <- data.frame(Authors  = character(n.items),
                        Title    = character(n.items),
                        Bookname = character(n.items),
                        Volume   = character(n.items),
                        Pages    = character(n.items),
                        Year     = character(n.items),
                        DOI      = character(n.items),
                        stringsAsFactors = FALSE)
  if (n.items){
    for (i in 1:n.items){
      item <- lapply(x$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`CAPITULOS-DE-LIVROS-PUBLICADOS`[[i]],
                     as.list)
      out.df$Title[i]    <- item$`DADOS-BASICOS-DO-CAPITULO`$`TITULO-DO-CAPITULO-DO-LIVRO`
      out.df$Bookname[i] <- item$`DETALHAMENTO-DO-CAPITULO`$`TITULO-DO-LIVRO`
      out.df$Volume[i]   <- item$`DETALHAMENTO-DO-CAPITULO`$`NUMERO-DE-VOLUMES`
      out.df$Year[i]     <- item$`DADOS-BASICOS-DO-CAPITULO`$ANO
      out.df$Pages[i]    <- paste0(item$`DETALHAMENTO-DO-CAPITULO`$`PAGINA-INICIAL`,
                                   "-",
                                   item$`DETALHAMENTO-DO-CAPITULO`$`PAGINA-FINAL`)
      out.df$DOI[i]      <- ifelse(item$`DADOS-BASICOS-DO-CAPITULO`$DOI == "" | item$`DADOS-BASICOS-DO-CAPITULO`$DOI == " ",
                                   paste0("zNotAvailable no.", ID, "-", i),
                                   item$`DADOS-BASICOS-DO-CAPITULO`$DOI)
      out.df$Authors[i]  <- get_authors(item)
    }
  }
  return(out.df)
}
