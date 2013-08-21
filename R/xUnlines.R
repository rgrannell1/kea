
#' Collapase a character vector with newlines as delimiters.
#'
#' @param strs a character vector.
#'
#' @return a length-one character vector.
#'
#' @export

#| function: xUnlines version: 0.1 finished: false

xUnlines <- function (strs) {
	# Collection str -> str;
	# collapse the collection of strs with a newline.

	pcall <- sys.call()
	require_a("collection_of_length_one", strs, pcall)

	strs <- unlist(strs)
	require_a("character", strs, pcall)

	paste0(strs, collapse = '\n')
}