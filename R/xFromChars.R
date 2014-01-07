
#' xFromChars
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
#' @rdname xFromChars
#' @export

xFromChars <- function (strs) {
	# Collection str -> str
	# collapse the collection strs with the empty str.

	xImplode("", strs)
}

#' @rdname xFromChars
#' @export

xFromChars... <- function (...) {
	xImplode("", list(...))
}
