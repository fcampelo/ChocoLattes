# download_lattes_CVs <- function(ID.vector, target.folder){
#
#   # Sanity checks
#   if(!is.character(ID.vector)) {
#     stop("ID.vector must be a character vector")
#   }
#   if(any(ID.vector != as.character(as.numeric(ID.vector)))) {
#     stop("All ID.vector entries must be composed of 16 numeric
#          characters, e.g., '6799982843395323'.\n Check entry(ies): ",
#          which(suppressWarnings(is.na(as.character(as.numeric(ID.vector))))))
#   }
#   if(any(nchar(ID.vector) != 16)) {
#     stop("All ID.vector entries must be valid Lattes ID,
#          i.e., a 16 numeric characters\n Check entry(ies): ",
#          which(nchar(ID.vector) != 16))
#   }
#
#   # Filter out repeated entries and
#   ID.vector <- unique(ID.vector)
#
#   # Download
#   for (i in seq_along(ID.vector)){
#     htmlPage <- XML::htmlTreeParse(paste0("http://buscacv.cnpq.br/buscacv/#/espelho?nro_id_cnpq_cp_s=",
#                                           ID.vector[i]),
#                                    useInternal = TRUE)
#     links <- xpathSApply(htmlPage, '//*[@id="content"]/section/ng-include[1]/div/div[2]/ul[1]/li[2]/a')
#   }
# }
