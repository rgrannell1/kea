
#' Collapase a character vector with empty strs as delimiters.
#'
#' @param ... several character vectors.
#'
#' @return a length-one character vector.
#'
#' @export

xUnchars <- function (...) {
	# Collection str -> str
	# collapse the collection strs with the empty str.

	pcall <- sys.call()
	strs <- lapply(c(...), dearrowise)
	
	assert(
		is_collection(strs), pcall,
		exclaim$must_be_collection(strs))

	strs <- coerce_to_vector(strs, 'character')

	if (length(strs) == 0) {
		character(0)
	} else {
		paste0(strs, collapse = '')		
	}
}
