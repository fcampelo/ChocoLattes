#' Extract accepted journal papers
#'
#' Extracts accepted journal papers from Lattes list.
#'
#' This function extracts relevant information on accepted journal papers from a Lattes list.
#'
#' @param x Lattes list (generated internally in [lattes_to_list()])
#' @param ID a unique identifier for each CV being processed.
#'
#' @return data frame containing parsed information on accepted journal papers

get_accepted_papers <- function(x,
                                ID = stats::runif(1)){
  n.items <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-ACEITOS-PARA-PUBLICACAO`)
  out.df  <- data.frame(Authors = character(n.items),
                        Title   = character(n.items),
                        Journal = character(n.items),
                        DOI     = character(n.items),
                        Year    = character(n.items),
                        stringsAsFactors = FALSE)
  if (n.items){
    for (i in 1:n.items){
      item <- lapply(x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-ACEITOS-PARA-PUBLICACAO`[[i]],
                     as.list)
      out.df$Title[i]   <- item$`DADOS-BASICOS-DO-ARTIGO`$`TITULO-DO-ARTIGO`
      out.df$Journal[i] <- item$`DETALHAMENTO-DO-ARTIGO`$`TITULO-DO-PERIODICO-OU-REVISTA`
      out.df$DOI[i]     <- ifelse(item$`DADOS-BASICOS-DO-ARTIGO`$DOI == "" | item$`DADOS-BASICOS-DO-ARTIGO`$DOI == " ",
                                    paste0("zNotAvailable no.", ID, "-", i),
                                    item$`DADOS-BASICOS-DO-ARTIGO`$DOI)
      out.df$Year[i]    <- item$`DADOS-BASICOS-DO-ARTIGO`$`ANO-DO-ARTIGO`
      out.df$Authors[i] <- get_authors(item)
    }
  }
  return(out.df)
}
