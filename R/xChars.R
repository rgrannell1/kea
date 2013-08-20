
#' Split a string at every character.
#'
#' @param string a length-one character vector.
#'
#' @return a character vector.
#'
#' @export

#| function: xChars version: 0.1 finished: false 

xChars <- function (string) {
	# string -> Vector string
	# split string at every character, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	
	require_a("string", string, pcall)

	if (nchar(string) == 0) {
		''
	} else {
		strsplit(string, "")[[1]]
	}
}
