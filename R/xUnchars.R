
#' Collapase a character vector with empty strs as delimiters.
#'
#' @param strs a collection of strs.
#'
#' @return a length-one character vector.
#'
#' @export

#| function: xUnchars version: 0.1 finished: false

xUnchars <- function (strs) {
	# Collection str -> str
	# collapse the collection strs with the empty str.

	pcall <- sys.call()
	require_a("collection_of_string", strs, pcall)

	if (length(strs) == 0) {
		character(0)
	} else {
		paste0(strs, collapse = '')		
	}
}
