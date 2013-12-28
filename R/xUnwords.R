
#' xUnwords
#'
#' Collapase a character vector with spaces as delimiters.
#'
#' @param
#'    strs an arbitrary number of character vectors.
#'
#' @return
#'    a length-one character vector.
#'
#' @family character_vector_functions
#'
#' @export

xUnwords <- function (strs) {
	# Collection str -> str
	# collapse the collection strs with a space.

	xImplode(" ", strs)
}

#' @export

xUnwords... <- function (...) {
	xImplode(" ", list(...))
}
