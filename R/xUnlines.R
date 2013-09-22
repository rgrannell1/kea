
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
		!missing(strs), pcall)

	assert(
		is.vector(strs) || is.pairlist(strs), pcall)
	assert(
		length(strs) == 0 || is.character(unlist(strs)), pcall)

	paste0(strs, collapse = '\n')
}