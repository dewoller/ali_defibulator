

python_Rify = function( file_path ) {

	# make sure the python code is loaded
	reticulate::source_python( file_path )

	signature = get_first_python_function_signature( file_path )
	function_name =  signature[[1]]
	function_args = paste( signature[[2]], collapse=',' )

	function_text =glue::glue(.open='<<', .close='>>', "

function( << function_args >> ) {

create_arg_list = function( args ) {

args %>% map_chr( function(x) {
	if( is.character(x) ) {
		str_c(\"'\",str_replace_all( x, \"'\", \"\\\\'\"),\"'\")
	} else {
		str_c(x)
	}
}) %>% str_c( collapse=',')

}

function_args_string = create_arg_list( list( <<function_args>> ))

		py_run_string(str_c('
# the following is python code
rv = << function_name >> ( ', function_args_string, '  )
'))
		return( reticulate::py$rv)

	}

	")

	assign(	 str_c('python_', function_name), eval( parse( text=function_text	)) , envir = .GlobalEnv)

}


if(FALSE) {
file_path = 'python/abc.py'
python_Rify( file_path )

python_asdf1

	# doesn't work yet
python_asdf1(list(1:2),2)

python_asdf1('a',2)


}

# Define a Python function to parse the Python file
get_first_python_function_signature <- function(file_path) {

  python_code <- sprintf('
import ast

def get_first_function_args(file_path):
    with open(file_path, "r") as source_code:
        tree = ast.parse(source_code.read())
        for node in tree.body:
            if isinstance(node, ast.FunctionDef):
                return (node.name, [arg.arg for arg in node.args.args])

    return []

result = get_first_function_args("%s")
  ', file_path)

  # Run the Python code
  reticulate::py_run_string(python_code)

  # Return the result
  return(reticulate::py$`result`)
}

