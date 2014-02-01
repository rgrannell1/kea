
#' xFromChars
#'
#' Concatentate a character vector with empty strs as delimiters.
#'
#' @param
#'    strs a collection of strings. The
#'    strings to collapse into one string.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A length-one character vector.
#'
#' @family character_functions
#'
#' @template
#'    Variadic
#'
#'
#' @example
#'    inst/examples/example-xFromChars.R
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
