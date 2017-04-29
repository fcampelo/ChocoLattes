#' Extract published conference papers
#'
#' Extracts published conference papers from Lattes list.
#'
#' This function extracts relevant information on published conference papers from a Lattes list.
#'
#' @param x Lattes list (generated internally in [lattes_to_list()])
#' @param ID a unique identifier for each CV being processed.
#'
#' @return data frame containing parsed information on conference papers

get_conference_papers <- function(x,
                                  ID = stats::runif(1)){
  n.items <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`)
  out.df  <- data.frame(Authors    = character(n.items),
                        Title      = character(n.items),
                        Conference = character(n.items),
                        Country    = character(n.items),
                        DOI        = character(n.items),
                        Pages      = character(n.items),
                        Year       = character(n.items),
                        Internac   = logical(n.items),
                        stringsAsFactors = FALSE)
  if (n.items){
    for (i in 1:n.items){
      item <- lapply(x$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`[[i]],
                     as.list)

      out.df$Title[i]      <- item$`DADOS-BASICOS-DO-TRABALHO`$`TITULO-DO-TRABALHO`
      out.df$Conference[i] <- item$`DETALHAMENTO-DO-TRABALHO`$`NOME-DO-EVENTO`
      out.df$Country[i]    <- item$`DADOS-BASICOS-DO-TRABALHO`$`PAIS-DO-EVENTO`
      out.df$Year[i]       <- max(item$`DETALHAMENTO-DO-TRABALHO`$`ANO-DE-REALIZACAO`,
                                  item$`DADOS-BASICOS-DO-TRABALHO`$`ANO-DO-TRABALHO`)
      out.df$Pages[i]      <- paste0(item$`DETALHAMENTO-DO-TRABALHO`$`PAGINA-INICIAL`,
                                     "-",
                                     item$`DETALHAMENTO-DO-TRABALHO`$`PAGINA-FINAL`)
      out.df$DOI[i]        <- ifelse(item$`DADOS-BASICOS-DO-TRABALHO`$DOI == "" | item$`DADOS-BASICOS-DO-TRABALHO`$DOI == " ",
                                     paste0("zNotAvailable no.", ID, "-", i),
                                     item$`DADOS-BASICOS-DO-TRABALHO`$DOI)
      out.df$Internac[i]   <- (item$`DETALHAMENTO-DO-TRABALHO`$`CLASSIFICACAO-DO-EVENTO` == "INTERNACIONAL")
      out.df$Authors[i]    <- get_authors(item)
    }
  }
  return(out.df)
}
