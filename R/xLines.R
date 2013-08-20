#' Split a string at newlines.
#'
#' @param string a length-one character vector.
#'     
#' @return a character vector, with one or more elements.
#'
#' @export

#| function: xLines version: 0.1 finished: false 

xLines <- function (string) {
	# string -> [string]
	# split string at every newline, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	
	require_a("string", string, pcall)

	strsplit(string, split = "\n+")[[1]]
}
