#' Extract QUALIS information
#'
#' Summarize production as classified by a given QUALIS extract
#'
#' @param lattes.list a Lattes list object created using [lattes_to_list()]
#' @param years integer vector with years to be extracted from lattes.list
#' @param qualis.file CSV file containing the ranking of journals according to
#' the QUALIS classification system for a certain area. See `Qualis File` for details.
#' @param isComputerScience logical flag to indicate if the relevant Qualis file
#' is related to the "Computer Science" area (in which case conferences also receive
#' QUALIS ranks)
#' @param output.file type of file to output (xlsx or csv).
#' @param plotQualis logical flag, should a plot be generated?
#' @param qualis.extract name of the qualis extract used (optional, for the plot only)
#' @param plot.width,plot.height,plot.units,plot.res,plot.text.size graphical parameters
#'
#' @section Qualis File:
#' The qualis file must be a CSV file with commas as separators and UTF-8
#' encoding. To ensure these properties, follow the steps:
#' - Generate the relevant file from
#' [https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf](https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf),
#' selecting only the fields _Evento de Classificacao_ and _Area de Avaliacao_
#' (leave the others blank).
#' - Download the resulting **xls** file.
#' - Open the file using your reader of choice, select all and copy
#' - Open a new spreadsheet on Google Docs, [https://docs.google.com/spreadsheets/u/0/](https://docs.google.com/spreadsheets/u/0/)
#' - Paste everything into the Google Docs Spreadsheet
#' - Download the resulting file as a comma-separated file (**File** -> **Download as** -> **Comma-separated values (.csv, current sheet)**)
#'
#' @return This routine returns nothing.
#' It generates one **.xlsx** or **csv** file for each year in `years`,
#' as well as a summary plot per year (as a png file), if `plotQualis = TRUE`
#' @export

extract_qualis <- function(lattes.list, years, qualis.file,
                           isComputerScience = FALSE,
                           output.file = c("csv", "xlsx"),
                           plotQualis = TRUE,
                           qualis.extract = NULL,
                           plot.width = 16, plot.height = 8,
                           plot.units = "in", plot.res = 300,
                           plot.text.size = 18){

  # read qualis file
  qualis <- utils::read.csv(qualis.file,
                            header = TRUE, sep = ",", encoding = "UTF-8",
                            stringsAsFactors = FALSE)
  qualis[, 2] <- toupper(qualis[, 2])

  for (year in years){
    # select year and extract relevant info from lattes.list
    tmplist <- lapply(lattes.list,
                      FUN = function(x, year){x[x$Year == year, ]},
                      year = year)
    titles   <- tmplist$`Journal Papers`$Title
    authors  <- unlist(tmplist$`Journal Papers`$Authors)
    journals <- toupper(tmplist$`Journal Papers`$Journal)

    if(isComputerScience){
      titles   <- c(titles, tmplist$`Conference Papers`$Title)
      authors  <- c(authors, tmplist$`Conference Papers`$Authors)
      journals <- c(journals, toupper(tmplist$`Conference Papers`$Conference))
    }

    myqualis <- data.frame(Authors    = authors,
                           Title      = titles,
                           Journal    = journals,
                           QUALIS     = character(length(journals)),
                           stringsAsFactors = FALSE)

    for (i in 1:length(journals)){
      jname <- myqualis$Journal[i]
      indx  <- which(jname == qualis[, 2])[1]
      if (length(indx) > 0){
        myqualis$QUALIS[i] <- qualis$Estrato[indx]
      } else myqualis$QUALIS[i] <- "N/A"
    }

    myqualis <- myqualis[order(myqualis$Journal), ]

    # output file
    if (output.file == "xlsx"){
    WriteXLS::WriteXLS(myqualis,
                       ExcelFileName = paste0("Productions x Qualis for year ",
                                              year, ".xlsx"),
                       Encoding      = "UTF-8",
                       row.names     = FALSE)
      } else if (output.file == "csv"){
        utils::write.csv(myqualis,
                         file = paste0("Productions x Qualis for year ",
                                       year, ".csv"),
                         quote = FALSE, row.names = FALSE)
      } else stop("output.file format not recognized. Please use 'csv' or 'xlsx'.")

    if (plotQualis){
      dfTab <- as.data.frame(table(myqualis$QUALIS))
      colnames(dfTab)[1] <- "x"
      mp <- ggplot2::ggplot(dfTab, ggplot2::aes(x     = dfTab$x,
                                                y     = dfTab$Freq,
                                                label = dfTab$Freq))

      grDevices::png(file = paste0("./Qualis distribution in year", year, ".png"),
                     width = plot.width, height = plot.height,
                     units = plot.units, res = plot.res)
      mp <- mp + ggplot2::geom_bar(stat = "identity") +
        ggplot2::xlab(paste0("Qualis (", qualis.extract, ")")) +
        ggplot2::ylab("Count") +
        ggplot2::theme(axis.title  = ggplot2::element_text(size = plot.text.size),
                       axis.text   = ggplot2::element_text(size = plot.text.size),
                       legend.text = ggplot2::element_text(size = plot.text.size)) +
        ggplot2::geom_text(vjust = 0, nudge_y = 0.5, size = plot.text.size * 0.7) +
        ggplot2::ylim(0, max(dfTab$Freq + 2))
      print(mp)
      grDevices::dev.off()
    }
  }
}
