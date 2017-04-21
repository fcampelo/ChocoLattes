## ChocoLattes package
[![Build Status](https://api.travis-ci.org/fcampelo/ChocoLattes.png)](https://travis-ci.org/fcampelo/ChocoLattes) 

[Felipe Campelo](mailto:fcampelo@ufmg.br)  
Operations Research and Complex Systems Laboratory - ORCS Lab
Universidade Federal de Minas Gerais
Belo Horizonte, Brazil

***

**R** package for extracting and summarising information from the [Lattes database of Brazilian Researchers](http://lattes.cnpq.br).

This package currently exports four methods:

- *lattes_to_list(...)* - extracts information from a Lattes CV xml file (or a list of xml files, or a folder full of xml files) and structures everything in a neat list vector, taking care of duplicates (using DOI and title for papers, and ISSN and title for books) and formatting author names. It currently extracts info on journal papers (accepted or published), conference papers, book chapters, books, PhD theses and MSc dissertations.
- *make_productions_page(...)* - builds a neat HTML page with the productions of the researcher (or lab, or department), based on the list produced by *lattes_to_list(...)*
- *plot_chart* produces a summary chart of productions, based on the list produced by *lattes_to_list(...)*
- *extract_qualis* generates an Excel (or CSV) file containing the production summary of the researcher (or lab, or department), based on the list produced by *lattes_to_list(...)* and on a certain [QUALIS extract](http://qualis.capes.gov.br/)

***
Example:

1. Download example file (_curriculo.xml_) from [here](http://buscatextual.cnpq.br/buscatextual/download.do?metodo=apresentar&idcnpq=6799982843395323), and save it to your current working folder.

2. Convert it to a Lattes list:

```
library(ChocoLattes) # <-- assuming it is installed, of course
lattes.list <- lattes_to_list(filenames = "curriculo.xml")
```

3. Build a productions chart

```
a <- plot_chart(lattes.list = lattes.list, 
                chart.type  = "ggplot2")
print(a)
```

or a full productions page:

```
make_productions_page(lattes.list = lattes.list, 
                      chart.type  = "plotly", 
                      h1.title    = "Felipe Campelo", 
                      h2.title    = "UFMG, Brazil",
                      language    = "EN")
```

Have fun!!!  
Felipe
