
#' Split a str at whitespace.
#'
#' @param s a length-one character vector.
#'     
#' @return a character vector, with one or more elements.
#'
#' @export

#| function: xWords version: 0.1 finished: false 

xWords <- function (str) {
	# str -> Vector str
	# split a str at every whitespace character, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	
	require_a("string", str, pcall)

	if (nchar(str) == 0) {
		''
	} else {
		strsplit(str, split = "[ \n\t]+")[[1]]
	}
}
