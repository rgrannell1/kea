
#' xFromLines
#'
#' Concatentate a character vector with newlines as delimiters.
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
#' @family text_processing_functions
#'
#' @template
#'    Variadic
#'
#'
#' @example
#'    inst/examples/example-xFromLines.R
#'
#' @rdname xFromLines
#' @export

xFromLines <- function (strs) {
	# Collection str -> str;
	# collapse the collection of strs with a newline.

	xImplode("\n", strs)
}

#' @rdname xFromLines
#' @export

xFromLines_ <- function (...) {
	xFromLines(list(...))
}
