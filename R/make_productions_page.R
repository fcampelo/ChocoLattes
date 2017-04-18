#' Generate HTML page with productions list and summary chart
#'
#' This function generates an HTML page with productions list and summary chart.
#' Use chart.type = "plotly" for an interactive plot, and "ggplot2"
#' for a static one. Option "rCharts" is currently disabled, until the rCharts
#' package ([https://github.com/ramnathv/rCharts](https://github.com/ramnathv/rCharts))
#' becomes available on CRAN.
#'
#' @param lattes.list a Lattes list object created using [lattes_to_list()]
#' @param chart.type package to use for generating the summary chart. "plotly" and
#' "rCharts" output interactive charts, "ggplot2" outputs a static one.
#' @param chart.width plot width (for "plotly" and "rCharts")
#' @param chart.height plot height (for "plotly")
#' @param h1.title H1 title for the page
#' @param h2.title H2 subtitle for the page
#'
#' @export

make_productions_page <- function(lattes.list,
                                  chart.type   = c("ggplot2", "plotly", "rCharts"),
                                  chart.width  = 960,
                                  chart.height = 480,
                                  h1.title     = "My Laboratory<br/>My Department<br/>My University",
                                  h2.title     = "Academic Productions"
                                  ){

  # Match argument
  chart.type <- match.arg(chart.type, c("ggplot2", "plotly", "rCharts"))

  # Save lattes.list as temporary file
  saveRDS(object = lattes.list, file = "./lattes_list.tmp")

  # Open md file for creating page
  md.file <- file("prod_page.Rmd",
                  open = "wt", encoding = "UTF-8")

  # =============== Write to md file =============== #
  writeLines("---\n    output: html_document\n---\n", con = md.file)
  writeLines(paste0("<h1>", h1.title, "</h1>"), con = md.file)
  writeLines(paste0("<h2>", h2.title, "</h2>\n<hr>\n\n"), con = md.file)

  writeLines("```{r setup, include=FALSE, echo=FALSE, warning=FALSE, error=FALSE}", con = md.file)
  writeLines("lattes.list <- readRDS('./lattes_list.tmp')", con = md.file)
  writeLines("Prod.Years <- lapply(lattes.list, FUN = function(x){unique(x$Year)})", con = md.file)
  writeLines("years <- sort(unique(unlist(Prod.Years)), decreasing = TRUE)", con = md.file)
  writeLines("```", con = md.file)

  writeLines("```{r, results='asis', echo=FALSE, comment=NA, tidy=FALSE, fig.align='center', fig.width=9}", con = md.file)
  writeLines("cat('<a name=\"pagetop\"></a>\\n\\n<p align=\"center\">', sep = \"\")", con = md.file)
  writeLines("for(i in 1:(length(years) - 1)){\ncat(\"[\", years[i], \"](#\",\nyears[i], \") | \",\nsep = \"\")\n}", con = md.file)
  writeLines("cat(\"[\", years[length(years)], \"](#\", years[length(years)], \")\\n\\n\", sep = \"\")", con = md.file)

  writeLines(paste0("plot_chart(lattes.list, chart.type = '", chart.type,
                    "', width = ", chart.width,
                    ", height = ", chart.height, ")"), con = md.file)
  writeLines("cat(\"</p>\")", con = md.file)

  writeLines("for (year in years){", con = md.file)
  writeLines("tmplist <- lapply(lattes.list, function(x,year){x[x$Year == year, ]}, year = year)", con = md.file)
  writeLines("cat('<a name=\"', year, '\"></a>\\n\\n', '## ', year, '\\n', sep = \"\")", con = md.file)
  writeLines("print_books(tmplist$Books)", con = md.file)
  writeLines("print_accepted(tmplist$`Accepted for Publication`)", con = md.file)
  writeLines("print_journal_papers(tmplist$`Journal Papers`)", con = md.file)
  writeLines("print_conferences(tmplist$`Conference Papers`, isIntl = TRUE)", con = md.file)
  writeLines("print_conferences(tmplist$`Conference Papers`, isIntl = FALSE)", con = md.file)
  writeLines("print_book_chapters(tmplist$`Book Chapters`)", con = md.file)
  writeLines("print_books(tmplist$Books)", con = md.file)
  writeLines("cat('<p align=\"right\">[Back to top](#pagetop)</p>')\n}\n```\n\n", con = md.file)

  writeLines("<div style=\"background-color:#eeeeee; width:600px\">", con = md.file)
  writeLines("Last updated: `r date()`<br/>", con = md.file)
  writeLines("Created with [ChocoLattes](https://github.com/fcampelo/ChocoLattes)<br/>", con = md.file)
  writeLines("[ORCS Lab](http://orcslab.ppgee.ufmg.br) - Operational Research and Complex Systems Laboratory<br/>", con = md.file)
  writeLines("Universidade Federal de Minas Gerais, Belo horizonte MG, Brazil\n</div>", con = md.file)

  close(md.file)

  # Render page
  rmarkdown::render(input = "./prod_page.Rmd")

  # remove temp files
  success <- all(file.remove(c("./lattes_list.tmp", "./prod_page.Rmd")))

  return(success)
}
