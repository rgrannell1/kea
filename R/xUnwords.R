
#' Collapase a character vector with spaces as delimiters.
#'
#' @param ... an arbitrary number of character vectors.
#'
#' @return a length-one character vector.
#'
#' @export

xUnwords <- function (...) {
	# Collection str -> str
	# collapse the collection strs with a space.

	xCollapse(" ", ...)
}
