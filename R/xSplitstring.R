
#' Split a string using a regexp.
#'
#' @param regexp a length-one character vector.
#  @param str a length-one character vector.
#'
#' @return a character vector.
#'
#' @export

#| function: xSplitstring version: 0.1 finished: false 

xSplitstring <- function (regexp, str) {
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
