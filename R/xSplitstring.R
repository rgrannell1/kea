
#' Split a string using a regexp.
#'
#' @param regexp a length-one character vector.
#  @param string a length-one character vector.
#'
#' @return a character vector.
#'
#' @export

#| function: xSplitstring version: 0.1 finished: false 

xSplitstring <- function (regexp, string) {
	# string -> string -> Vector string
	# split a string into substrings at a regexp.
	
	pcall <- sys.call()
	require_a("string", regexp, pcall)
	require_a("string", string, pcall)

	if (nchar(string) == 0) {
		''
	} else {
		strsplit(string, regexp)[[1]]		
	}		
}
