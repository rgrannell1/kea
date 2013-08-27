
#' Split a str at every character.
#'
#' @param str a length-one character vector.
#'
#' @return a character vector.
#'
#' @export

#| function: xChars version: 0.1 finished: false 

xChars <- function (str) {
	# str -> Vector str
	# split str at every character, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	
	require_a("string", str, pcall)

	if (nchar(str) == 0) {
		''
	} else {
		strsplit(str, "")[[1]]
	}
}