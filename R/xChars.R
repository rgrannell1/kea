
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
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xChars <- function (str) {
	# str -> Vector str
	# split str at every character, returning 
	# a character vector of equal or greater length.
	
	pcall <- sys.call()	

	assert(
		!missing(str), pcall)

	assert(length(str) %in% c(0, 1), pcall)
	assert(
		length(str) == 0 || is.character(unlist(str)), pcall)

	if (length(str) == 0) {
		character(0)
	} else if (nchar(str) == 0) {
		""
	} else {
		strsplit(str, "")[[1]]
	}
}
