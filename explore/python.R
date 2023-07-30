
install.packages('kableExtra')

install.packages(c("officer", "flextable", "rvest", "xml2"))

library(officer)
library(flextable)
library(rvest)
library(xml2)

# read in	the docx file
# extract tables, convert to html

# https://davidgohel.github.io/officer/articles/offcran/tables.html

# https://davidgohel.github.io/officer/articles/powerpoint.html

document_path = 'doc/clinical_documentation/1196350047 - Emma Louise Franklin 267026_Emma_Louise_Franklin_CDI_Emma_Franklin_3626889_381172601.docx'  # replace with your document path

    tables = extract_tables_from_docx(document_path)
    # print the first table
    print(tables[0])
    # print the entire html
    print( extract_tables_from_docx(document_path,TRUE)


library(reticulate)
source_python("extract_tables.py")



read_docx(docfiles[4]) %>%
  body_extract_tbl() -> tables_in_doc

flextables <- list()

for (i in seq_along(tables_in_doc)) {
  # Convert each table to a flextable and store in the list
  flextables[[i]] <- flextable::flextable(tables_in_doc[[i]])
}

html_tables <- list()

for (i in seq_along(flextables)) {
  # Convert each flextable to HTML and store in the list
  html_tables[[i]] <- flextables[[i]] %>% 
    as.html() %>% 
    html_table(fill = TRUE)
}

for (i in seq_along(html_tables)) {
  # Save each HTML table to a file
  write_html(html_tables[[i]], paste0("table_", i, ".html"))
}


docx_summary() %>% 
	as_tibble() %>%

	filter( text != '') %>%
	count( content_type, style_name ) %>%
	knitr::kable() %>% 
	kableExtra::kable_styling() 
