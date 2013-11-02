
#' xUnchars
#'
#' Collapase a character vector with empty strs as delimiters.
#'
#' @param ... several character vectors.
#'
#' @return a length-one character vector.
#'
#' @export

xUnchars <- function (...) {
	# Collection str -> str
	# collapse the collection strs with the empty str.

	xCollapse("", ...)
}
