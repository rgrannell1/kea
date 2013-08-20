
#' Collapase a character vector with empty strings as delimiters.
#'
#' @param strings a collection of strings.
#'
#' @return a length-one character vector.
#'
#' @export

#| function: xUnchars version: 0.1 finished: false

xUnchars <- function (strings) {
	# Collection string -> string
	# collapse the collection strings with the empty string.

	pcall <- sys.call()
	require_a("listy", strings, pcall)

	strings <- unlist(strings)
	
	require_a("character", strings, pcall)

	paste0(strings, collapse = '')
}
