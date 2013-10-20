
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
		!missing(strs), pcall,
		exclaim$parameter_missing(strs))

	strs <- dearrowise(strs)

	assert(
		is_collection(strs), pcall,
		exclaim$must_be_collection(strs))

	strs <- coerce_to_typed_vector(strs, 'character')

	paste0(strs, collapse = '\n')
}