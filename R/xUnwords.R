
#' Collapase a character vector with spaces as delimiters.
#'
#' @param strs a character vector, or list or pairlist of strs.
#'
#' @return a length-one character vector.
#'
#' @export

#| function: xUnwords version: 0.1 finished: false

xUnwords <- function (strs) {
	# Collection str -> str
	# collapse the collection strs with a space.

	pcall <- sys.call()
	require_a("collection_of_length_one", strs, pcall)

	strs <- unlist(strs)
	
	require_a("character", strs, pcall)

	paste0(strs, collapse = ' ')
}