
#' xChars
#' 
#' Split a single string into a vector of characters.
#'
#' @param str a length-one character vector.
#'
#' @return a character vector of length \code{nchar(str)}.
#'
#' @template glossary
#'
#' @examples 
#' @export

xChars <- function (str) {
	# str -> Vector str
	# split str at every character, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	
	assert(
		is.character(str) && length(str) == 1, pcall)

	if (nchar(str) == 0) {
		""
	} else {
		strsplit(str, "")[[1]]
	}
}
