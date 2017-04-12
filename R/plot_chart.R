# Function to plot summary chart

plot_chart <- function(lattes.list, type = "ggplot2"){
  # preprocess lattes.list
  lattes.list$`Conference Papers - International` <- lattes.list$`Conference Papers`[which(lattes.list$`Conference Papers`$Internac), ]
  lattes.list$`Conference Papers - National`      <- lattes.list$`Conference Papers`[which(!lattes.list$`Conference Papers`$Internac), ]
  lattes.list$`Conference Papers`                 <- NULL

  # Assemble data frame with total counts per year
  myTotals              <- lapply(lattes.list,
                                  function(x){as.data.frame(table(x$Year))})
  myTotals              <- do.call(rbind.data.frame, myTotals)
  myTotals$Type         <- gsub("\\..*","", rownames(myTotals))
  names(myTotals)       <- c("Year", "Count", "Type")
  myTotals$Year         <- as.numeric(as.character(myTotals$Year))
  myTotals              <- myTotals[order(myTotals$Year), ]
  rownames(myTotals)    <- paste0(myTotals$Year, myTotals$Type)

  # Second data frame containing all entries by year (even if with 0 occurrences)
  myTotals2          <- with(myTotals,
                             expand.grid(Year     = unique(Year),
                                         Type     = unique(Type)))
  myTotals2$Count    <- 0
  rownames(myTotals2) <- paste0(myTotals2$Year, myTotals2$Type)

  # Populate counts for myTotals2
  for (i in 1:nrow(myTotals)){
    indx <- which(rownames(myTotals2) == rownames(myTotals)[i])
    if(length(indx)){
      myTotals2$Count[indx] <- myTotals$Count[i]
    }
  }


  ## Using ggplot2
  if (type == "ggplot2" | type == "ggplotly"){
    myPlot <- ggplot2::ggplot(data    = myTotals2,
                              mapping = ggplot2::aes(x     = Year,
                                                     y     = Count,
                                                     fill  = Type)) +
      ggplot2::geom_bar(stat     = "identity",
                        position = "stack",
                        colour   = "#00000011") +
      ggplot2::scale_x_continuous(breaks = min(myTotals2$Year):max(myTotals2$Year)) +
      ggplot2::theme(axis.text.x     = ggplot2::element_text(angle = 45, vjust = 0),
                     legend.position = "bottom")

    # STOPPED HERE
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
