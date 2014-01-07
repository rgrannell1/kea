
#' xFromWords
#'
#' Collapase a character vector with spaces as delimiters.
#'
#' @param
#'    strs an arbitrary number of character vectors.
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
#' @rdname xFromWords
#' @export

xFromWords <- function (strs) {
	# Collection str -> str
	# collapse the collection strs with a space.

	xImplode(" ", strs)
}

#' @rdname xFromWords
#' @export

xFromWords... <- function (...) {
	xImplode(" ", list(...))
}
