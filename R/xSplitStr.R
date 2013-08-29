
#' xSplitStr
#' 
#' Split a string into a character vector using a regular expression.
#'
#' @param rexp a regular expression.
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

#| function: xSplitStr version: 0.1 finished: false 

xSplitStr <- function (rexp, str) {
	# str -> str -> Vector str
	# split a str into substrs at a rexp.
	
	pcall <- sys.call()
	require_a("string", rexp, pcall)
	require_a("string", str, pcall)

	if (nchar(str) == 0) {
		''
	} else {
		strsplit(str, rexp)[[1]]		
	}		
}
