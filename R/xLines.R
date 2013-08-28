
#' xLines
#' 
#' Split a string at every newline character.
#'
#' @param str a string.
#'
#' @return a character vector.
#'
#' @template glossary
#'
#' @examples 
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
