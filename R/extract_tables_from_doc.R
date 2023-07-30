extract_tables_from_docx = function( document_path, html= TRUE) {

	# Pass the parameter to the Python function and convert tables to HTML
	py_run_string(paste0("tables = extract_tables_from_docx('", document_path, "')"))

	if( html) {
		py_run_string("html_tables = convert_tables_to_html(tables)")
		py$html_tables
	} else {
		py$tables
	}
}




