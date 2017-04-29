#' Extract published books
#'
#' Extracts published books from Lattes list
#'
#' This function extracts relevant information on published books from a Lattes list
#'
#' @param x Lattes list (generated internally in [lattes_to_list()])
#' @param ID a unique identifier for each CV being processed.
#'
#' @return data frame containing parsed information on published books

get_books <- function(x,
                      ID = stats::runif(1)){
  n.items <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`)
  out.df  <- data.frame(Authors   = character(n.items),
                        Bookname  = character(n.items),
                        Publisher = character(n.items),
                        Edition   = character(n.items),
                        City      = character(n.items),
                        Pages     = character(n.items),
                        Year      = character(n.items),
                        ISBN      = character(n.items),
                        stringsAsFactors = FALSE)
  if (n.items){
    for (i in 1:n.items){
      item <- lapply(x$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`[[i]],
                     as.list)
      out.df$Bookname[i]  <- item$`DADOS-BASICOS-DO-LIVRO`$`TITULO-DO-LIVRO`
      out.df$Publisher[i] <- item$`DETALHAMENTO-DO-LIVRO`$`NOME-DA-EDITORA`
      out.df$Edition[i]   <- item$`DETALHAMENTO-DO-LIVRO`$`NUMERO-DA-EDICAO-REVISAO`
      out.df$City[i]      <- paste0(item$`DETALHAMENTO-DO-LIVRO`$`CIDADE-DA-EDITORA`,
                                    ", ",
                                    item$`DADOS-BASICOS-DO-LIVRO`$`PAIS-DE-PUBLICACAO`)
      out.df$Pages[i]     <- item$`DETALHAMENTO-DO-LIVRO`$`NUMERO-DE-PAGINAS`
      out.df$Year[i]      <- item$`DADOS-BASICOS-DO-LIVRO`$ANO
      out.df$ISBN[i]      <- ifelse(item$`DETALHAMENTO-DO-LIVRO`$ISBN == "" | item$`DETALHAMENTO-DO-LIVRO`$ISBN == " ",
                                    paste0("zNotAvailable no.", ID, "-", i),
                                    item$`DETALHAMENTO-DO-LIVRO`$ISBN)
      out.df$Authors[i]   <- get_authors(item)
    }
  }
  return(out.df)
}
