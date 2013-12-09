
#' xUnchars
#'
#' Collapase a character vector with empty strs as delimiters.
#'
#' @param
#'    strs several character vectors.
#'
#' @return
#'    a length-one character vector.
#'
#' @export

xUnchars <- function (strs) {
	# Collection str -> str
	# collapse the collection strs with the empty str.

	xCollapse("", strs)
}

#' @export

xUnchars... <- function (...) {
	xCollapse("", list(...))
}
