
#' xAsCharacter
#'
#' Convert a collection to a double vector.
#'
#' @details
#'    \code{xAsCharacter} converts a list, pairlist or vector of
#'    length-one strings to a character vector. It does not attempt
#'    to convert non-character collections to character vectors.
#'
#' @param
#'    strs a collection of strings. A list, pairlist or vector
#'    of length-one character vectors to convert to a character vector.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A character vector.
#
#' @template
#'    Variadic
#'
#' @rdname xAsCharacter
#' @export

xAsCharacter <- function (strs) {
	# Collection integer -> Vector integer
	# convert a collection to a integer vector.

	invoking_call <- sys.call()

	assert(
		!missing(strs), invoking_call,
		exclaim$parametre_missing(strs))

	insist$must_be_collection(strs, invoking_call)

	as_typed_vector(strs, 'character')
}

#' @rdname xAsCharacter
#' @export

xAsCharacter... <- function (...) {
	xAsCharacter(list(...))
}

