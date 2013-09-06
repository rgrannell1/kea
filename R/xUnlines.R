
#' Collapase a character vector with newlines as delimiters.
#'
#' @param strs a character vector.
#'
#' @return a length-one character vector.
#'
#' @export

xUnlines <- function (strs) {
	# Collection str -> str;
	# collapse the collection of strs with a newline.

	pcall <- sys.call()
	require_a("collection_of_string", strs, pcall)

	paste0(strs, collapse = '\n')
}