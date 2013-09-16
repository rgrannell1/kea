
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

	pcall <- sys.call()
	strs <- c(...)
	
	assert(
		is.vector(strs) || is.pairlist(strs), pcall)

	assert(
		is.character(strs), pcall)

	paste0(strs, collapse = ' ')
}
