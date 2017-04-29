#' Convert a set of Lattes CV XML files to a list object
#'
#' Extract information from a set of Lattes XML files and convert it to a list
#' vector
#'
#' This function extracts relevant information from a set of Lattes CV XML files
#' and outputs a list object containing specific information on the following
#' aspects of a group's production:
#' - Accepted journal papers
#' - Published journal papers
#' - Published conference papers
#' - Published book chapters
#' - Published books
#' - Ph.D. student defenses
#' - M.Sc. student defenses
#'
#' Journal and conference papers are checked for duplication using DOI and Title
#' information. Duplicated entries are registered only once.
#' @param CV.dir folder where CVs are contained. If NULL
#' then the current working directory is used.
#' @param author.aliases list vector with author aliases.
#' See \code{Examples} for details.
#'
#' @return list vector where each element is a dataframe with information on a
#' specific aspect of the academic production
#'
#' @export
#'
#' @examples
#' my.dir <- system.file("extdata", package="ChocoLattes")
#'
#' # Define the aliases of authors "Felipe Campelo" and "Lucas S. Batista":
#' # (all aliases will be converted to the first name provided for each author)
#' my.aliases <- list(c("Felipe Campelo",
#'                      "Felipe Campelo Franca Pinto",
#'                      "Felipe Campelo F. Pinto",
#'                      "F.C.F. Pinto"),
#'                    c("Lucas S. Batista",
#'                      "Lucas Batista",
#'                      "Lucas de Souza Batista",
#'                      "Lucas Souza Batista"))
#'
#' lattes.list <- lattes_to_list(CV.dir         = my.dir,
#'                               author.aliases = my.aliases)

lattes_to_list <- function(CV.dir    = NULL,
                           author.aliases = list()){

  # Standardize CV.dir
  if(!R.utils::isAbsolutePath(CV.dir)){
    CV.dir <- paste0(getwd(), "/",
                     gsub(pattern = "[.]/", "", CV.dir))
  }

  # get filenames
  filenames <- paste0(CV.dir, "/", dir(CV.dir, pattern = ".xml"))
  filenames <- gsub("//", "/", filenames)

  # Prepare list for results
  out.list        <- vector("list", 7)
  names(out.list) <- c("Accepted for Publication",
                       "Journal Papers",
                       "Conference Papers",
                       "Book Chapters",
                       "Books",
                       "MSc Dissertations",
                       "PhD Theses")

  for (indx in seq_along(filenames)){
    # Read XML to a list object
    x <- XML::xmlToList(XML::xmlTreeParse(filenames[indx],
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
  out.list <- lapply(out.list, FUN = capitalize_authors, author.aliases = author.aliases)

  # Get good capitalization of Titles
  out.list <- lapply(out.list, FUN = capitalize_titles)

  # Remove duplicated works (by DOI, ISSN or Title)
  out.list <- lapply(out.list, FUN = remove_duplicates)

  return(out.list)
}
