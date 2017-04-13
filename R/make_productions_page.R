#' @param lattes.list a Lattes list object created using [lattes_to_list()]
#' @param chart.type package to use for generating the summary chart. "plotly" and
#' "rCharts" output interactive charts, "ggplot2" outputs a static one.
#' @param width plot width (for "plotly" and "rCharts")
#' @param height plot height (for "plotly")

make_productions_page <- function(lattes.list,
                                  chart.type   = c("ggplot2", "plotly", "rCharts"),
                                  chart.width  = 960,
                                  chart.height = 480,
                                  h1.title     = "My Laboratory<br/>My Department<br/>My University",
                                  h2.title     = "Academic Productions"
                                  ){

  # Match argument
  type <- match.arg(type, c("ggplot2", "plotly", "rCharts"))

  # Save lattes.list as temporary file
  saveRDS(object = lattes.list, file = "./lattes_list.tmp")

  # Open md file for creating page
  md.file <- file("prod_page.Rmd",
                  open = "wt", encoding = "UTF-8")

  # Write to md file
  writeLines("---\n    output: html_document\n---\n", con = md.file)
  writeLines(cat("<h1>",h1.title,"</h1>", sep = ""), con = md.file)
  writeLines(cat("<h2>",h2.title,"</h2>\n<hr>", sep = ""), con = md.file)

  writeLines("```{r setup, include=FALSE, echo=FALSE, warning=FALSE, error=FALSE}", con = md.file)
  writeLines("lattes.list <- readRDS('./lattes_list.tmp')")
  writeLines("Prod.Years <- lapply(lattes.list, FUN = function(x){unique(x$Year)})", con = md.file)
  writeLines("years <- sort(unique(unlist(Prod.Years)), decreasing = TRUE)", con = md.file)
  writeLines("```", con = md.file)

  writeLines("```{r, results='asis', echo=FALSE, comment=NA, tidy=FALSE, fig.align='center', fig.width=9}", con = md.file)
  writeLines("cat('<a name=\"pagetop\"></a>\n\n<p align=\"center\">', sep = \"\")", con = md.file)
  writeLines("for(i in 1:(length(Myears) - 1)){\ncat(\"[\", Myears[i], \"](#\",\nMyears[i], \") | \",\nsep = \"\")\n}", con = md.file)
  writeLines("cat(\"[\", Myears[length(Myears)], \"](#\",\nMyears[length(Myears)], \")\n\n\",\nsep = \"\")", con = md.file)

  writeLines(cat("plot_chart(lattes.list, chart.type = '", chart.type,
                 "', width = ", chart.width,
                 ", height = ", chart.height, ")",
                 sep = ""), con = md.file)
  writeLines("cat(\"</p>\")", con = md.file)

  # STOPPED HERE =====================

  for (year in Myears){
    tmplist <- lapply(Mypubs, FUN = .selectyear, year = year)
    cat('<a name="', year, '"></a>\n\n',
        '## ', year, '\n',
        sep = "")

    # Print that year's works to markdown and HTML
    .printBooks(tmplist$Books)
    .printAccepted(tmplist$Accepted)
    .printPapers(tmplist$Papers)
    .printConfs(tmplist$Confs, isIntl = TRUE)
    .printConfs(tmplist$Confs, isIntl = FALSE)
    .printChaps(tmplist$Chaps)
    cat('<p align="right">[Back to top](#pagetop)</p>')
  }


  close(md.file)

  # remove temp file
  file.remove("./lattes_list.tmp")






}
