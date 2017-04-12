

make_productions_page <- function(lattes.list, chart.type){

  # Extract years with nonzero production
  Prod.Years <- lapply(lattes.list,
                       FUN = function(x){unique(x$Year)})
  years      <- sort(unique(unlist(Prod.Years)),
                     decreasing = TRUE)

  # Open md file for creating page
  md.file <- file("prod_page.md",
                  open = "wt", encoding = "UTF-8")

  # Write to md file
  writeLines(text = '<a name="pagetop"></a>\n\n<p align="center">',
             con = md.file)

  for(i in 1:(length(years) - 1)){
    writeLines(text = cat("[", years[i], "](#",
                          years[i], ") | ",
                          sep = ""),
               con = md.file)
  }
  writeLines(text = cat("[", years[length(years)], "](#",
                        years[length(years)], ")\n\n",
                        sep = ""),
             con = md.file)

  # Plot chart of productions
  #a <- plotChart(lattes.list, type = chart.type)

  # then writelines(a)


  close(md.file)


}
