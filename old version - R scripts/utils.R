
# ==========================================
# Function to extract entries from a given year
.selectyear <- function(x, year){
    return(x[x$Year == year, ])
}



# ==========================================
# Function to extract and format accepted papers
.getAccepted <- function(x, k){
    npap     <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-ACEITOS-PARA-PUBLICACAO`)
    MyAccept <- data.frame(Authors = character(npap),
                           Title   = character(npap),
                           Journal = character(npap),
                           DOI     = character(npap),
                           Year    = character(npap),
                           stringsAsFactors = FALSE)
    if (npap){
        for (i in 1:npap){
            paper <- x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-ACEITOS-PARA-PUBLICACAO`[[i]]
            MyAccept$Title[i]   <- tools::toTitleCase(tolower(paper$`DADOS-BASICOS-DO-ARTIGO`[[2]]))
            MyAccept$Journal[i] <- paper$`DETALHAMENTO-DO-ARTIGO`[[1]]
            MyAccept$DOI[i]     <- ifelse(paper$`DADOS-BASICOS-DO-ARTIGO`[[9]] == "",
                                          paste0("zNotAvailable no.", k, "-", i),
                                          paper$`DADOS-BASICOS-DO-ARTIGO`[[9]])
            MyAccept$Year[i]    <- paper$`DADOS-BASICOS-DO-ARTIGO`[[3]]
            authindx            <- which(names(paper) == "AUTORES")
            nauthors            <- length(authindx)
            MyAccept$Authors[i] <- .authorname(paper[[authindx[1]]][1])
            if (nauthors > 1){
                for (j in 2:nauthors){
                    MyAccept$Authors[i] <- paste0(MyAccept$Authors[i],
                                                  ", ",
                                                  .authorname(paper[[authindx[j]]][1]))
                }
            }
        }
    }
    return(MyAccept)
}



# ==========================================
# Function to extract and format published papers
.getPapers <- function(x, k){
    npap     <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`)
    MyPapers <- data.frame(Authors = character(npap),
                           Title   = character(npap),
                           Journal = character(npap),
                           Volume  = character(npap),
                           Issue   = character(npap),
                           Pages   = character(npap),
                           Year    = character(npap),
                           DOI     = character(npap),
                           stringsAsFactors = FALSE)

    # Probably more effective if I used lapply, but more didactic with a for loop
    if (npap){
        for (i in 1:npap){
            paper <- x$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`[[i]]
            MyPapers$Title[i]   <- tools::toTitleCase(tolower(paper$`DADOS-BASICOS-DO-ARTIGO`[[2]]))
            MyPapers$Journal[i] <- paper$`DETALHAMENTO-DO-ARTIGO`[[1]]
            MyPapers$Volume[i]  <- paper$`DETALHAMENTO-DO-ARTIGO`[[3]]
            MyPapers$Issue[i]   <- ifelse(paper$`DETALHAMENTO-DO-ARTIGO`[[5]] == "",
                                          yes = "1",
                                          no  = paper$`DETALHAMENTO-DO-ARTIGO`[[5]])
            MyPapers$Pages[i]   <- paste0(paper$`DETALHAMENTO-DO-ARTIGO`[[6]],
                                          "-",
                                          paper$`DETALHAMENTO-DO-ARTIGO`[[7]])
            MyPapers$Year[i]    <- paper$`DADOS-BASICOS-DO-ARTIGO`[[3]]
            MyPapers$DOI[i]     <- ifelse(paper$`DADOS-BASICOS-DO-ARTIGO`[[9]] == "",
                                          paste0("zNotAvailable no.", k, "-", i),
                                          paper$`DADOS-BASICOS-DO-ARTIGO`[[9]])
            authindx            <- which(names(paper) == "AUTORES")
            nauthors            <- length(authindx)
            MyPapers$Authors[i] <- .authorname(paper[[authindx[1]]][1])
            if (nauthors > 1){
                for (j in 2:nauthors){
                    MyPapers$Authors[i] <- paste0(MyPapers$Authors[i],
                                                  ", ",
                                                  .authorname(paper[[authindx[j]]][1]))
                }
            }
        }
    }
    return(MyPapers)
}



# ==========================================
# Function to extract and format conference papers
.getConfs <- function(x, k){
    npap    <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`)
    MyConfs <- data.frame(Authors    = character(npap),
                          Title      = character(npap),
                          Conference = character(npap),
                          Country    = character(npap),
                          DOI        = character(npap),
                          Pages      = character(npap),
                          Year       = character(npap),
                          Internac   = logical(npap),
                          stringsAsFactors = FALSE)

    if (npap){
        for (i in 1:npap){
            paper <- x$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`[[i]]
            MyConfs$Title[i]      <- tools::toTitleCase(tolower(paper$`DADOS-BASICOS-DO-TRABALHO`[[2]]))
            MyConfs$Conference[i] <- paper$`DETALHAMENTO-DO-TRABALHO`[[2]]
            MyConfs$Country[i]    <- paper$`DADOS-BASICOS-DO-TRABALHO`[[4]]
            MyConfs$Year[i]       <- max(paper$`DETALHAMENTO-DO-TRABALHO`[[4]],
                                         paper$`DADOS-BASICOS-DO-TRABALHO`[[3]])
            MyConfs$Pages[i]      <- paste0(paper$`DETALHAMENTO-DO-TRABALHO`[[9]],
                                            "-",
                                            paper$`DETALHAMENTO-DO-TRABALHO`[[10]])
            MyConfs$DOI[i]        <- ifelse(paper$`DADOS-BASICOS-DO-TRABALHO`[[9]] == "",
                                            paste0("zNotAvailable no.", k, "-", i),
                                            paper$`DADOS-BASICOS-DO-TRABALHO`[[9]])
            MyConfs$Internac[i]   <- (paper$`DETALHAMENTO-DO-TRABALHO`[[1]] == "INTERNACIONAL")
            authindx              <- which(names(paper) == "AUTORES")
            nauthors              <- length(authindx)
            MyConfs$Authors[i]    <- .authorname(paper[[authindx[1]]][1])
            if (nauthors > 1){
                for (j in 2:nauthors){
                    MyConfs$Authors[i] <- paste0(MyConfs$Authors[i],
                                                 ", ",
                                                 .authorname(paper[[authindx[j]]][1]))
                }
            }
        }
    }
    return(MyConfs)
}



# ==========================================
# Function to extract and format book chapters
.getChaps <- function(x, k){
    npap    <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`CAPITULOS-DE-LIVROS-PUBLICADOS`)
    MyChaps <- data.frame(Authors  = character(npap),
                          Title    = character(npap),
                          Bookname = character(npap),
                          Volume   = character(npap),
                          Pages    = character(npap),
                          Year     = character(npap),
                          DOI      = character(npap),
                          stringsAsFactors = FALSE)

    if (npap){
        for (i in 1:npap){
            paper <- x$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`CAPITULOS-DE-LIVROS-PUBLICADOS`[[i]]
            MyChaps$Title[i]    <- tools::toTitleCase(tolower(paper$`DADOS-BASICOS-DO-CAPITULO`[[2]]))
            MyChaps$Bookname[i] <- tools::toTitleCase(tolower(paper$`DETALHAMENTO-DO-CAPITULO`[[1]]))
            MyChaps$Volume[i]   <- paper$`DETALHAMENTO-DO-CAPITULO`[[2]]
            MyChaps$Year[i]     <- paper$`DADOS-BASICOS-DO-CAPITULO`[[3]]
            MyChaps$Pages[i]    <- paste0(paper$`DETALHAMENTO-DO-CAPITULO`[[3]],
                                          "-",
                                          paper$`DETALHAMENTO-DO-CAPITULO`[[4]])
            MyChaps$DOI[i]      <- ifelse(paper$`DADOS-BASICOS-DO-CAPITULO`[[9]] == "",
                                          paste0("zNotAvailable no.", k, "-", i),
                                          paper$`DADOS-BASICOS-DO-CAPITULO`[[9]])
            authindx            <- which(names(paper) == "AUTORES")
            nauthors            <- length(authindx)
            MyChaps$Authors[i]  <- .authorname(paper[[authindx[1]]][1])
            if (nauthors > 1){
                for (j in 2:nauthors){
                    MyChaps$Authors[i] <- paste0(MyChaps$Authors[i],
                                                 ", ",
                                                 .authorname(paper[[authindx[j]]][1]))
                }
            }
        }
    }
    return(MyChaps)
}



# ==========================================
# Function to extract and format books
.getBooks <- function(x, k){
    npap    <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`)
    MyBooks <- data.frame(Authors   = character(npap),
                          Bookname  = character(npap),
                          Publisher = character(npap),
                          Edition   = character(npap),
                          City      = character(npap),
                          Pages     = character(npap),
                          Year      = character(npap),
                          ISBN      = character(npap),
                          stringsAsFactors = FALSE)

    if (npap){
        for (i in 1:npap){
            paper <- x$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`[[i]]
            MyBooks$Bookname[i]  <- tools::toTitleCase(tolower(paper$`DADOS-BASICOS-DO-LIVRO`[[3]]))
            MyBooks$Publisher[i] <- paper$`DETALHAMENTO-DO-LIVRO`[[7]]
            MyBooks$Edition[i]   <- paper$`DETALHAMENTO-DO-LIVRO`[[4]]
            MyBooks$City[i]      <- paste0(paper$`DETALHAMENTO-DO-LIVRO`[[6]],
                                           ", ",
                                           paper$`DADOS-BASICOS-DO-LIVRO`[[5]])
            MyBooks$Pages[i]     <- paper$`DETALHAMENTO-DO-LIVRO`[[2]]
            MyBooks$Year[i]      <- paper$`DADOS-BASICOS-DO-LIVRO`[[4]]
            MyBooks$ISBN[i]      <- ifelse(paper$`DETALHAMENTO-DO-LIVRO`[[3]] == "",
                                           paste0("zNotAvailable no.", k, "-", i),
                                           paper$`DETALHAMENTO-DO-LIVRO`[[3]])
            authindx             <- which(names(paper) == "AUTORES")
            nauthors             <- length(authindx)
            MyBooks$Authors[i]   <- .authorname(paper[[authindx[1]]][1])
            if (nauthors > 1){
                for (j in 2:nauthors){
                    MyBooks$Authors[i] <- paste0(MyBooks$Authors[i],
                                                 ", ",
                                                 .authorname(paper[[authindx[j]]][1]))
                }
            }
        }
    }

    return(MyBooks)
}




# ==========================================
# Function to print accepted papers
.printAccepted <- function(x){
    npap <- nrow(x)
    if(npap){
        cat("### Papers Accepted for Publication\n")
        for (i in 1:npap){
            if(x$DOI[i] != ""){
              cat(i, ". ",
                  x$Authors[i], ": _",
                  "[", x$Title[i], "](http://dx.doi.org/", x$DOI[i], ")", "._ ",
                  x$Journal[i], ", ",
                  x$Year[i],
                  sep = "")
            }
            else{
              url <- paste0('http://scholar.google.com.br/scholar?hl=pt-BR&q=', x$Title[i], "&as_sauthors=", x$Authors[i], sep="")
              cat(i, ". ",
                  x$Authors[i], ": _",
                  "[", x$Title[i], "]", "(", url, ")", "._ ",
                  x$Journal[i], ", ",
                  x$Year[i],
                  sep = "")
            }
            cat("\n\n<hr>",
                sep = "")
        }
    }
}


# ==========================================
# Function to print journal papers
.printPapers <- function(x){
    npap <- nrow(x)
    if(npap){
        cat("### Journal Papers\n")
        for (i in 1:nrow(x)){
            if(x$DOI[i] != ""){
              cat(i, ". ",
                  x$Authors[i],
                  ": _",
                  "[", x$Title[i], "](http://dx.doi.org/", x$DOI[i], ")", "._ ",
                  x$Journal[i], " ",
                  x$Volume[i],
                  "(", x$Issue[i], ")",
                  sep = "")
            }
            else{
              url <- paste0('http://scholar.google.com.br/scholar?hl=pt-BR&q=', x$Title[i], "&as_sauthors=", x$Authors[i], sep="")
              cat(i, ". ",
                  x$Authors[i],
                  ": _",
                  "[", x$Title[i], "]", "(", url, ")", "._ ",
                  x$Journal[i], " ",
                  x$Volume[i],
                  "(", x$Issue[i], ")",
                  sep = "")
            }
            if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
                cat(":", x$Pages[i],
                    sep = "")
            }
            cat(", ", x$Year[i],
                sep = "")
            cat("\n\n<hr>",
                sep = "")
        }
    }
}


# ==========================================
# Function to print conference papers
.printConfs <- function(x, isIntl = TRUE){
  x <- x[which(x$Internac == isIntl), ]
  npap <- nrow(x)
  if(npap){
    cat("### Conference Proceedings -",
        ifelse(isIntl,
               "International\n",
               "National/Regional\n"))
    for (i in 1:nrow(x)){
      if( x$DOI[i] != ""){
            cat(i, ". ",
            	x$Authors[i], ": _",
            	"[", x$Title[i], "](http://dx.doi.org/", x$DOI[i], ")", "._ ",
            	x$Conference[i], ", ",
            	sep = "")
      }
      else{
        url <- paste0('http://scholar.google.com.br/scholar?hl=pt-BR&q=', x$Title[i], "&as_sauthors=", x$Authors[i], sep="")
            cat(i, ". ",
            	x$Authors[i], ": _",
            	"[", x$Title[i], "]", "(", url, ")", "._ ",
            	x$Conference[i], ", ",
            	sep = "")
      }
      if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
          cat("pp.", x$Pages[i], ", ",
              sep = "")
      }
      cat(x$Year[i],
          sep = "")
      cat("\n\n<hr>",
          sep = "")
    }
  }
}

# ==========================================
# Function to print book chapters
.printChaps <- function(x){
  npap <- nrow(x)
  if(npap){
    cat("### Book Chapters\n")
    for (i in 1:nrow(x)){
      if( x$DOI[i] != ""){
        cat(i, ". ",
            x$Authors[i],
            ": _",
            "[", x$Title[i], "](http://dx.doi.org/", x$DOI[i], ")", "._ ",
            "In: ", x$Bookname[i], ", ",
            "vol. ", ifelse(x$Volume == "",
                            "1",
                            x$Volume),
            sep = "")
        }
        else{
          url <- paste0('http://scholar.google.com.br/scholar?hl=pt-BR&q=', x$Title[i], "&as_sauthors=", x$Authors[i], sep="")
          cat(i, ". ",
              x$Authors[i],
              ": _",
              "[", x$Title[i], "]", "(", url, ")", "._ ",
              "In: ", x$Bookname[i], ", ",
              "vol. ", ifelse(x$Volume == "",
                                "1",
                                x$Volume),
                sep = "")
        }
        if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
                cat(". pp. ,", x$Pages[i], ", ",
                    sep = "")
            }
            cat(x$Year[i],
                sep = "")
            cat("\n\n<hr>",
                sep = "")
        }
    }
}



# ==========================================
# Function to print books
.printBooks <- function(x){
    npap <- nrow(x)
    if(npap){
        cat("### Books\n")
        for (i in 1:nrow(x)){
            cat(i, ". ",
                x$Authors[i],
                ": _", x$Bookname[i], "_. ",
                sep = "")
            if(x$Publisher[i] != ""){
                cat(x$Publisher[i], ", ",
                    sep = "")
            }
            if(x$City[i] != ""){
                cat(x$City[i], ". ",
                    sep = "")
            }
            if(x$Pages[i] != ""){
                cat(x$Pages[i], " pages. ",
                    sep = "")
            }
            cat(x$Year[i], ". ",
                sep = "")
            if(x$ISBN[i] != ""){
                cat("ISBN: ", x$ISBN[i], ".",
                    sep = "")
            }
            cat("\n\n<hr>",
                sep = "")
        }
    }
}



# ==========================================
# Function to plot summary chart
.plotChart <- function(x){
    x$`Conference Papers - International`     <- x$`Conference Papers`[which(x$`Conference Papers`$Internac), ]
    x$`Conference Papers - National/Regional` <- x$`Conference Papers`[which(!x$`Conference Papers`$Internac), ]
    x$`Conference Papers` <- NULL
    myTotals              <- lapply(x, function(X){as.data.frame(table(X$Year))})
    myTotals              <- do.call(rbind.data.frame, myTotals)
    myTotals$Type         <- gsub("\\..*","",rownames(myTotals))
    names(myTotals)       <- c("Year", "Count", "Type")
    myTotals$Year         <- as.numeric(as.character(myTotals$Year))
    myTotals              <- myTotals[order(myTotals$Year), ]
    myTotals2             <- with(myTotals,
                                  expand.grid(Year     = unique(Year),
                                              Type     = unique(Type)))
    myTotals2$Count    <- 0

    rownames(myTotals2) <- paste0(myTotals2$Year, myTotals2$Type)
    rownames(myTotals)  <- paste0(myTotals$Year, myTotals$Type)
    for (i in 1:nrow(myTotals)){
        indx <- which(rownames(myTotals2) == rownames(myTotals)[i])
        if(length(indx)){
            myTotals2$Count[indx] <- myTotals$Count[i]
        }
    }

    # Using ggplot2 + plotly
    # myPlot <- ggplot(data    = myTotals2,
    # 								 mapping = aes(x     = Year,
    # 								 							y     = Count,
    # 								 							fill  = Type)) +
    # 	geom_bar(stat     = "identity",
    # 					 position = "stack",
    # 					 colour   = "#00000011") +
    # 	scale_x_continuous(breaks = min(myTotals2$Year):max(myTotals2$Year)) +
    # 	theme(axis.text.x     = element_text(angle = 45, vjust = 0),
    # 				legend.position = "bottom")
    #
    # #myPlotly <- ggplotly(myPlot, width = 960, height = 480)
    # return(myPlot)

    # Using rCharts
    TotalsPlot <- nPlot(Count ~ Year,
                        group = "Type",
                        data  = myTotals2,
                        type  = "multiBarChart")
    TotalsPlot$chart(color        = RColorBrewer::brewer.pal(8, "Set1"),
                     reduceXTicks = FALSE)
    TotalsPlot$xAxis(rotateLabels = -45, staggerLabels = FALSE)
    TotalsPlot$chart(multibar.stacked = TRUE)
    TotalsPlot$set(width = 800)
    TotalsPlot$print('iframesrc', include_assets = TRUE)
}
