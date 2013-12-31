
#' xUnchars
#'
#' Collapase a character vector with empty strs as delimiters.
#'
#' @param
#'    strs several character vectors.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a length-one character vector.
#'
#' @family character_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xUnchars
#' @export

xUnchars <- function (strs) {
	# Collection str -> str
	# collapse the collection strs with the empty str.

	xImplode("", strs)
}

#' @rdname xUnchars
#' @export

xUnchars... <- function (...) {
	xImplode("", list(...))
}
