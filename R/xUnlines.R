
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
	assert(
		is.vector(strs) || is.pairlist(strs) && 
		is.character(unlist(strs)), pcall)

	paste0(strs, collapse = '\n')
}