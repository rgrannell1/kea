
#' Collapase a character vector with spaces as delimiters.
#'
#' @param ... an arbitrary number of character vectors.
#'
#' @return a length-one character vector.
#'
#' @export

#| function: xUnwords version: 0.1 finished: false

xUnwords <- function (...) {
	# Collection str -> str
	# collapse the collection strs with a space.

	pcall <- sys.call()
	strs <- c(...)
	require_a("collection_of_string", strs, pcall)

	paste0(..., collapse = ' ')
}
