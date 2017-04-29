#' Extract advised PhD theses
#'
#' Extracts advised PhD theses from Lattes list.
#'
#' This function extracts relevant information on advised PhD theses from a Lattes list.
#'
#' @param x Lattes list (generated internally in [lattes_to_list()])
#' @param ID a unique identifier for each CV being processed.
#'
#' @return data frame containing parsed information on advised PhD theses

get_advised_theses <- function(x,
                               ID = stats::runif(1)){
  which.phd <- which(names(x$`OUTRA-PRODUCAO`$`ORIENTACOES-CONCLUIDAS`) == "ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO")
  n.items   <- length(which.phd)
  out.df    <- data.frame(Author        = character(n.items),
                          Title         = character(n.items),
                          Year          = character(n.items),
                          Language      = character(n.items),
                          Advisor       = rep(x$`DADOS-GERAIS`$.attrs[1],
                                              times = n.items),
                          IsMainAdvisor = logical(n.items),
                          Institution   = character(n.items),
                          Course        = character(n.items),
                          stringsAsFactors = FALSE)
  if (n.items){
    x <- x$`OUTRA-PRODUCAO`$`ORIENTACOES-CONCLUIDAS`[which.phd]
    for (i in 1:n.items){
      item <- lapply(x[[i]], as.list)
      out.df$Author[i]        <- item$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO`$`NOME-DO-ORIENTADO`
      out.df$Title[i]         <- item$`DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO`$TITULO
      out.df$Year[i]          <- item$`DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO`$ANO
      out.df$Language[i]      <- item$`DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO`$IDIOMA
      out.df$IsMainAdvisor[i] <- (item$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO`$`TIPO-DE-ORIENTACAO` == "ORIENTADOR_PRINCIPAL")
      out.df$Institution[i]   <- item$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO`$`NOME-DA-INSTITUICAO`
      out.df$Course[i]        <- item$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO`$`NOME-DO-CURSO`
    }
  }
  return(out.df)
}
