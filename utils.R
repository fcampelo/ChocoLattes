## Auxiliary functions for Lattes XML to HTML
# by Felipe Campelo (fcampelo@ufmg.br ; http://github.com/fcampelo)

# Load (and, if needed, install) required packages
required.packages <- c("XML")
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
	return(x[order(x$Year, decreasing = decreasing), ])
}


# ==========================================
# Function to remove duplicated entries
.removeduplicates <- function(x){
	# Remove duplicates (by DOI) and clean up placeholder values
	if ("DOI" %in% names(x)){
		unique.indx    <- as.numeric(rownames(unique(data.frame(x$DOI)[1])))
		x              <- x[unique.indx, ]
		na.indx        <- grep(pattern = "NotAvailable", x$DOI)
		x$DOI[na.indx] <- ""
	}
	
	# Remove duplicates (by ISSN) and clean up placeholder values
	if ("ISSN" %in% names(x)){
		unique.indx    <- as.numeric(rownames(unique(data.frame(x$ISBN)[1])))
		x              <- x[unique.indx, ]
		na.indx        <- grep(pattern = "NotAvailable", x$ISBN)
		x$ISBN[na.indx] <- ""
	}
	
	# Remove duplicates (by Title)
	if ("Title" %in% names(x)){
		titles      <- gsub("\\s", "", tolower(x$Title))
		unique.indx <- as.numeric(rownames(unique(data.frame(titles)[1])))
		x           <- x[unique.indx, ]
	}
	
	return (x)
}