#' Split a str at newlines.
#'
#' @param str a length-one character vector.
#'	 
#' @return a character vector, with one or more elements.
#'
#' @export

#| function: xLines version: 0.1 finished: false 

xLines <- function (str) {
	# str -> Vector str
	# split str at every newline, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	
	require_a("string", str, pcall)

	if (nchar(str) == 0) {
		''
	} else {
		strsplit(str, split = "\n+")[[1]]
	}
}
