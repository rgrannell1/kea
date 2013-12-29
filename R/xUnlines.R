
#' xUnlines
#'
#' Collapase a character vector with newlines as delimiters.
#'
#' @param
#'    strs a character vector.
#'
#' @return
#'    a length-one character vector.
#'
#' @family character_functions
#'
#' @export

xUnlines <- function (strs) {
	# Collection str -> str;
	# collapse the collection of strs with a newline.

	xImplode("\n", strs)
}

#' @export

xUnlines... <- function (...) {
	xUnlines(list(...))
}
