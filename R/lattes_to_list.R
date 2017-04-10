#' @param filenames character vector containing name of the file(s) to be processed. If NULL then use all XML files in the folder given by `CV.dir`
#' @param CV.dir folder where CVs are contained, relative to current working directory (**do not** use `.\` to refer the the current folder). If NULL then the current working directory is used.

lattes_to_list <- function(filenames = NULL,
                           CV.dir    = NULL){

  CV.dir <- paste0(getwd(), "/", CV.dir)
  if (is.null(filenames)) {
    filename <- dir(CV.dir, pattern = ".xml")
  }

  myLattes <- paste0(CV.dir, "/", filename)

  # Prepare list for results
  out.list        <- vector("list", 7)
  names(out.list) <- c("Accepted for Publication",
                       "Journal Papers",
                       "Conference Papers",
                       "Book Chapters",
                       "Books",
                       "MSc Dissertations",
                       "PhD Theses")

  for (indx in seq_along(myLattes)){
    # Read XML to a list object
    x <- XML::xmlToList(XML::xmlTreeParse(myLattes[indx],
                                          useInternal = TRUE,
                                          encoding    = "latin"))

    # Get productions
    MyPapers <- get_journal_papers(x, ID = indx)
    MyAccept <- get_accepted_papers(x, ID = indx)
    MyConfs  <- get_conference_papers(x, ID = indx)
    MyChaps  <- get_book_chapters(x, ID = indx)
    MyBooks  <- get_books(x, ID = indx)
    MyMsc    <- get_advised_dissertations(x, ID = indx)
    MyPhd    <- get_advised_theses(x, ID = indx)

    # ==========================================

    if (indx == 1) {
      out.list[[1]]   <- MyAccept
      out.list[[2]]   <- MyPapers
      out.list[[3]]   <- MyConfs
      out.list[[4]]   <- MyChaps
      out.list[[5]]   <- MyBooks
      out.list[[6]]   <- MyMsc
      out.list[[7]]   <- MyPhd
    } else {
      out.list[[1]]   <- rbind(out.list[[1]], MyAccept)
      out.list[[2]]   <- rbind(out.list[[2]], MyPapers)
      out.list[[3]]   <- rbind(out.list[[3]], MyConfs)
      out.list[[4]]   <- rbind(out.list[[4]], MyChaps)
      out.list[[5]]   <- rbind(out.list[[5]], MyBooks)
      out.list[[6]]   <- rbind(out.list[[6]], MyMsc)
      out.list[[7]]   <- rbind(out.list[[7]], MyPhd)
    }
  }

  # Sort: most recent first
  out.list <- lapply(out.list, FUN = sort_papers)

  # Get good capitalization of authornames
  out.list <- lapply(out.list, FUN = capitalize_authors)

  # Remove duplicated works (by DOI, ISSN or Title)
  out.list <- lapply(out.list, FUN = remove_duplicates)

  return(out.list)
}
