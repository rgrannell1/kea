
#' xFromLines
#'
#' Collapase a character vector with newlines as delimiters.
#'
#' @param
#'    strs a character vector.
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
#' @rdname xFromLines
#' @export

xFromLines <- function (strs) {
	# Collection str -> str;
	# collapse the collection of strs with a newline.

	xImplode("\n", strs)
}

#' @rdname xFromLines
#' @export

xFromLines... <- function (...) {
	xFromLines(list(...))
}
