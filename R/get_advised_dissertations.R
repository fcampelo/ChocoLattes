#' Extract advised MSc dissertations
#'
#' Extracts advised MSc dissertations from Lattes list.
#'
#' This function extracts relevant information on advised MSc dissertations from a Lattes list.
#'
#' @param x Lattes list (generated internally in [lattes_to_list()])
#' @param ID a unique identifier for each CV being processed.
#'
#' @return data frame containing parsed information on advised MSc dissertations

get_advised_dissertations <- function(x,
                                      ID = stats::runif(1)){
  which.msc <- which(names(x$`OUTRA-PRODUCAO`$`ORIENTACOES-CONCLUIDAS`) == "ORIENTACOES-CONCLUIDAS-PARA-MESTRADO")
  n.items   <- length(which.msc)
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
    x <- x$`OUTRA-PRODUCAO`$`ORIENTACOES-CONCLUIDAS`[which.msc]
    for (i in 1:n.items){
      item <- lapply(x[[i]], as.list)
      out.df$Author[i]        <- item$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`$`NOME-DO-ORIENTADO`
      out.df$Title[i]         <- item$`DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`$TITULO
      out.df$Year[i]          <- item$`DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`$ANO
      out.df$Language[i]      <- item$`DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`$IDIOMA
      out.df$IsMainAdvisor[i] <- (item$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`$`TIPO-DE-ORIENTACAO` == "ORIENTADOR_PRINCIPAL")
      out.df$Institution[i]   <- item$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`$`NOME-DA-INSTITUICAO`
      out.df$Course[i]        <- item$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`$`NOME-DO-CURSO`
    }
  }
  return(out.df)
}
