
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
#' @export

xUnwords <- function (strs) {
	# Collection str -> str
	# collapse the collection strs with a space.

	xCollapse(" ", strs)
}

#' @export

xUnwords... <- function (...) {
	xCollapse(" ", list(...))
}
