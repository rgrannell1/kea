
#' xSplitString
#' 
#' Split a string into a character vector using a regular expression.
#'
#' @param regexp a regular expression.
#' @param str a string.
#'
#' @return a character vector.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

#| function: xSplitString version: 0.1 finished: false 

xSplitString <- function (regexp, str) {
	# str -> str -> Vector str
	# split a str into substrs at a regexp.
	
	pcall <- sys.call()
	require_a("string", regexp, pcall)
	require_a("string", str, pcall)

	if (nchar(str) == 0) {
		''
	} else {
		strsplit(str, regexp)[[1]]		
	}		
}
