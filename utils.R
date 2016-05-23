## Auxiliary functions for Lattes XML to HTML
# by Felipe Campelo (fcampelo@ufmg.br ; http://github.com/fcampelo)

# Load (and, if needed, install) required packages
required.packages <- c("XML", "tools", "ggplot2", "plotly")#"devtools", "RColorBrewer")
install_and_load_packages <- function(pkg){
    if (!(pkg %in% rownames(installed.packages()))){
        install.packages(pkg)
    }
    require(pkg, character.only = TRUE)
}
ignore <- lapply(required.packages, install_and_load_packages)

# Installing from github
#if (!("rCharts" %in% rownames(installed.packages()))) install_github("ramnathv/rCharts")
#require(rCharts)

# ==========================================
# Function to easily capitalize author lists
.authorCap <- function(x) {
    if("Authors" %in% names(x) && nrow(x)){
        for (i in 1:nrow(x)){
            s <- strsplit(x$Authors[i], " ")[[1]]
            s <- paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
                       sep = "", collapse = " ")
            s <- gsub(pattern     = " De ", 
                      replacement = " de ", 
                      x           = s)
            s <- gsub(pattern     = " Da ", 
                      replacement = " da ", 
                      x           = s)
            
            # Replace my full name with my publication alias. Change if needed.
            s <- gsub(pattern     = "Felipe Campelo Franca Pinto", 
                      replacement = "Felipe Campelo", 
                      x           = s)
            
            s <- gsub(pattern     = "(\\.{0}[a-z]\\.)", 
                      replacement = "\\U\\1", 
                      x           = s,
                      perl        = TRUE)
            
            x$Authors[i] <- s
        }
    }
    return (x)
}


# ==========================================
# Function to get author names in "Name Surname" format
.authorname <- function(x){
    s <- strsplit(x, ",")[[1]]
    if (length(s) == 2) s <- paste(s[2], s[1])
    sub("^\\s+", "", s)
}



# Function to sort publications by year
.sortpapers <- function(x, decreasing = TRUE){
    if ("DOI" %in% names(x)){
        x <- x[order(x$DOI), ]
    }
    if ("ISBN" %in% names(x)){
        x <- x[order(x$ISBN), ]
    }
    return(x[order(x$Year, decreasing = decreasing), ])
}


# ==========================================
# Function to remove duplicated entries
.removeduplicates <- function(x){
    # Remove duplicates (by DOI) and clean up placeholder values
    if ("DOI" %in% names(x)){
        unique.indx    <- as.numeric(rownames(unique(data.frame(x$DOI)[1])))
        x              <- x[unique.indx, ]
        na.indx        <- grep(pattern = "zNotAvailable", x$DOI)
        x$DOI[na.indx] <- ""
    }
    
    # Remove duplicates (by ISBN) and clean up placeholder values
    if ("ISBN" %in% names(x)){
        unique.indx    <- as.numeric(rownames(unique(data.frame(x$ISBN)[1])))
        x              <- x[unique.indx, ]
        na.indx        <- grep(pattern = "zNotAvailable", x$ISBN)
        x$ISBN[na.indx] <- ""
    }
    
    # Remove duplicates (by Title)
    if ("Title" %in% names(x)){
        titles      <- gsub("\\s", "", tolower(x$Title))
        unique.indx <- as.numeric(rownames(unique(data.frame(titles)[1])))
        x           <- x[unique.indx, ]
    }
    
    # Remove inconsistencies in field "Year" (errors in the .xml due to typos)
    if("Year" %in% names(x)){
        ok.years <- as.numeric(x$Year) <= as.numeric(format(Sys.time(), "%Y"))
        x        <- x[ok.years, ]
    }
    
    return (x)
}


# ==========================================
# Function to extract entries from a given year
.selectyear <- function(x, year){
    return(x[x$Year == year, ])
}



# ==========================================
# Function to extract and format accepted papers
.getAccepted <- function(x){
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
                                          paste0("zNotAvailable no.", i),
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
.getPapers <- function(x){
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
                                          paste0("zNotAvailable no.", i),
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
.getConfs <- function(x){
    npap    <- length(x$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`)
    MyConfs <- data.frame(Authors    = character(npap),
                          Title      = character(npap),
                          Conference = character(npap),
                          Country    = character(npap),
                          DOI        = character(npap),
                          Pages      = character(npap),
                          Year       = character(npap),
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
                                            paste0("zNotAvailable no.", i),
                                            paper$`DADOS-BASICOS-DO-TRABALHO`[[9]])
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
.getChaps <- function(x){
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
                                          paste0("zNotAvailable no.", i),
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
.getBooks <- function(x){
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
                                           paste0("zNotAvailable no.", i),
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
# Function to plot summary chart
.plotChart <- function(x){
    myTotals           <- lapply(x, function(X){as.data.frame(table(X$Year))})
    myTotals           <- do.call(rbind.data.frame, myTotals)
    myTotals$Type      <- gsub("\\..*","",rownames(myTotals))
    names(myTotals)    <- c("Year", "Count", "Type")
    myTotals$Year      <- as.numeric(as.character(myTotals$Year))
    myTotals           <- myTotals[order(myTotals$Year), ]
    myTotals2          <- with(myTotals, 
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
    
    myPlot <- ggplot(data    = myTotals2, 
                     mapping = aes(x     = Year, 
                                   y     = Count, 
                                   fill  = Type)) + 
        geom_bar(stat     = "identity", 
                 position = "stack", 
                 colour   = "#00000011") + 
        scale_x_continuous(breaks = min(myTotals2$Year):max(myTotals2$Year)) + 
        theme(axis.text.x     = element_text(angle = 45, vjust = 0),
              legend.position = "bottom")
    
    #myPlotly <- ggplotly(myPlot, width = 960, height = 480)
    return(myPlot)
    
    # TotalsPlot <- nPlot(Count ~ Year, 
    #                     group = "Type", 
    #                     data  = myTotals2, 
    #                     type  = "multiBarChart")
    # TotalsPlot$chart(color        = RColorBrewer::brewer.pal(8, "Set1"), 
    #                  reduceXTicks = FALSE)
    # TotalsPlot$xAxis(rotateLabels=-90)
    # TotalsPlot$set(width = 800)
    # TotalsPlot$print('iframesrc', cdn = TRUE, include_assets = TRUE)
}




# ==========================================
# Function to print accepted papers
.printAccepted <- function(x){
    npap <- nrow(x)
    if(npap){
        cat("### Papers Accepted for Publication\n")
        for (i in 1:npap){
            cat(i, ". ",
                x$Authors[i], ": _",
                x$Title[i], "._ ",
                x$Journal[i], ", ",
                x$Year[i],
                sep = "")
            if(x$DOI[i] != ""){
                cat(".<br/>[[DOI: ",
                    x$DOI[i],
                    "](http://dx.doi.org/",
                    x$DOI[i], ")]",
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
            cat(i, ". ",
                x$Authors[i],
                ": _", x$Title[i], "._ ",
                x$Journal[i], " ",
                x$Volume[i], 
                "(", x$Issue[i], ")",
                sep = "")
            if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
                cat(":", x$Pages[i], 
                    sep = "")
            }
            cat(", ", x$Year[i], 
                sep = "")
            if(x$DOI[i] != ""){
                cat(".<br/>[[DOI: ",
                    x$DOI[i],
                    "](http://dx.doi.org/",
                    x$DOI[i], ")]",
                    sep = "")
            }
            cat("\n\n<hr>",
                sep = "")
        }
    }
}


# ==========================================
# Function to print conference papers
.printConfs <- function(x){
    npap <- nrow(x)
    if(npap){
        cat("### Conference Proceedings\n")
        for (i in 1:nrow(x)){
            cat(i, ". ",
                x$Authors[i],
                ": _", x$Title[i], "._ ",
                x$Conference[i], ", ",
                sep = "")
            if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
                cat("pp. ", x$Pages[i], 
                    sep = "")
            }
            cat(", ", x$Year[i], 
                sep = "")
            if(x$DOI[i] != ""){
                cat(".<br/>[[DOI: ",
                    x$DOI[i],
                    "](http://dx.doi.org/",
                    x$DOI[i], ")]",
                    sep = "")
            }
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
            cat(i, ". ",
                x$Authors[i],
                ": _", x$Title[i], "._ ",
                "In: ", x$Bookname[i], ", ",
                "vol. ", ifelse(x$Volume == "",
                                "1",
                                x$Volume),
                sep = "")
            if(length(grep("[0-9]+-[0-9]+$", x$Pages[i]))){
                cat(". pp. ", x$Pages[i], 
                    sep = "")
            }
            cat(", ", x$Year[i], 
                sep = "")
            if(x$DOI[i] != ""){
                cat(".<br/>[[DOI: ",
                    x$DOI[i],
                    "](http://dx.doi.org/",
                    x$DOI[i], ")]",
                    sep = "")
            }
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
            cat(x$Year, ". ",
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