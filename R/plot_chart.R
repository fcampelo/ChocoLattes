# Function to plot summary chart

# TO DO

plot_chart <- function(x, type = "rcharts"){
  x$ConfsIntl       <- x$Confs[which(x$Confs$Internac), ]
  x$ConfsNac        <- x$Confs[which(!x$Confs$Internac), ]
  x$Confs            <- NULL
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
  levels(myTotals2$Type) <- c("Art. Conferências Internacionais",
                              "Art. Periódicos",
                              "Art. Conferências Nacionais/Regionais",
                              "Livros",
                              "Capítulos de Livros",
                              "Artigos Aceitos para Publicação")

  ## Using ggplot2
  if (type == "ggplot2" | type == "ggplotly"){
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
    if (type == "ggplotly"){
      myPlot <- ggplotly(myPlot, width = 960, height = 480)
    }
    return(myPlot)
  }

  if(type == "rcharts"){
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
}
