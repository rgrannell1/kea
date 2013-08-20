
#' Collapase a character vector with empty strings as delimiters.
#'
#' @param strings a collection of strings.
#'
#' @return a string.
#'
#' @export

#| function: xUnchars version: 0.1 finished: false

xUnchars <- function (strings) {
	# character -> string
	# collapse the collection strings with the empty string.

	pcall <- sys.call()
	require_a("character", strings, pcall)

	paste0(strings, collapse = '')
}
