
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
		is_collection(strs), pcall,
		exclaim$must_be_collection(strs))

	strs <- coerce_to_vector(strs, 'character')

	paste0(strs, collapse = '\n')
}