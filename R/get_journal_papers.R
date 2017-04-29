#' Extract published journal papers
#'
#' Extracts published journal papers from Lattes list.
#'
#' This function extracts relevant information on published journal papers from a Lattes list.
#'
#' @param x Lattes list (generated internally in [lattes_to_list()])
#' @param ID a unique identifier for each CV being processed.
#'
#' @return data frame containing parsed information on journal papers

get_journal_papers <- function(x,
                               ID = stats::runif(1)){
  n.items <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`)
  out.df  <- data.frame(Authors = character(n.items),
                        Title   = character(n.items),
                        Journal = character(n.items),
                        Volume  = character(n.items),
                        Issue   = character(n.items),
                        Pages   = character(n.items),
                        Year    = character(n.items),
                        DOI     = character(n.items),
                        stringsAsFactors = FALSE)
  if (n.items){
    for (i in 1:n.items){
      item <- lapply(x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`[[i]],
                     as.list)
      out.df$Title[i]   <- item$`DADOS-BASICOS-DO-ARTIGO`$`TITULO-DO-ARTIGO`
      out.df$Journal[i] <- item$`DETALHAMENTO-DO-ARTIGO`$`TITULO-DO-PERIODICO-OU-REVISTA`
      out.df$Volume[i]  <- item$`DETALHAMENTO-DO-ARTIGO`$VOLUME
      out.df$Issue[i]   <- ifelse(item$`DETALHAMENTO-DO-ARTIGO`$FASCICULO == "",
                                    yes = "1",
                                    no  = item$`DETALHAMENTO-DO-ARTIGO`$FASCICULO)
      out.df$Year[i]    <- item$`DADOS-BASICOS-DO-ARTIGO`$`ANO-DO-ARTIGO`
      out.df$DOI[i]     <- ifelse(item$`DADOS-BASICOS-DO-ARTIGO`$DOI == "" | item$`DADOS-BASICOS-DO-ARTIGO`$DOI == " ",
                                    paste0("zNotAvailable no.", ID, "-", i),
                                  item$`DADOS-BASICOS-DO-ARTIGO`$DOI)
      out.df$Pages[i]   <- paste0(item$`DETALHAMENTO-DO-ARTIGO`$`PAGINA-INICIAL`,
                                    "-",
                                  item$`DETALHAMENTO-DO-ARTIGO`$`PAGINA-FINAL`)
      out.df$Authors[i] <- get_authors(item)
    }
  }
  return(out.df)
}
