
#' xSplitStr
#' 
#' Split a string into a character vector using a regular expression.
#'
#' @param rexp a regular expression.
#' @param str a string.
#'P
#' @return a character vector.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xSplitStr <- function (rexp, str) {
	# Vector string -> Vector string -> Vector str
	# split a str into substrs at a rexp.
	
	pcall <- sys.call()

	assert(is.character(rexp) && length(rexp) == 1, pcall)
	assert(is.character(str) && length(str) == 1, pcall)

	if (nchar(str) == 0) {
		''
	} else {
		strsplit(str, rexp)[[1]]		
	}
}
