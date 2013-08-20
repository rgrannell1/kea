
#' Split a string at whitespace.
#'
#' @param s a length-one character vector.
#'     
#' @return a character vector, with one or more elements.
#'
#' @export

#| function: xWords version: 0.1 finished: false 

xWords <- function (string) {
	# string -> [string]
	# split a string at every whitespace character, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	
	require_a("string", string, pcall)

	strsplit(string, split = "[ \n\t]+")[[1]]
}
