## Auxiliary functions for Lattes XML to HTML
# by Felipe Campelo (fcampelo@ufmg.br ; http://github.com/fcampelo)

# Load (and, if needed, install) required packages
required.packages <- c("XML", "tools")
install_and_load_packages <- function(pkg){
    if (!(pkg %in% rownames(installed.packages()))){
        install.packages(pkg)
    }
    require(pkg, character.only = TRUE)
}
ignore <- lapply(required.packages, install_and_load_packages)

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
# Function to print accepted papers
.selectyear <- function(x, year){
    return(x[x$Year == year, ])
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
                ": **", x$Bookname[i], "**. ",
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