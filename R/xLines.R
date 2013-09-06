
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

xLines <- function (str) {
	# str -> Vector str
	# split str at every newline, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	
	require_a(c('length_zero character', 'length_one character'), str, pcall)

	if (length(str) == 0) {
		character(0)
	} else if (nchar(str) == 0) {
		''
	} else {
		strsplit(str, split = "\n+")[[1]]
	}
}
