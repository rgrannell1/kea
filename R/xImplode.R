
#' xImplode
#'
#' Concatenate a character vector into a single string using a delimiter.
#'
#' @param
#'    str a string to use as a delimiter.
#'
#' @param
#'    strs several character vectors.
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
#' @rdname xImplode
#' @export

xImplode <- function (str, strs) {
	# string -> Vector string -> string
	# Collapse a collection of strs into
	# a str with by a delim.

	invoking_call <- sys.call()

	assert(
		!missing(str), invoking_call,
		exclaim$parametre_missing(str))

	insist$must_be_collection(strs, invoking_call)
	insist$must_be_collection(str, invoking_call)

	str <- as_typed_vector(str, 'character', True)
	strs <- as_typed_vector(strs, 'character')

	insist$must_be_of_length(str, 0:1, invoking_call)

	if (length(strs) == 0) {
		character()
	} else {

		paste(
			strs[
				nchar(strs) != 0 &
				vapply(strs, length, integer(1)) != 0],
			collapse = str)
	}
}

#' @rdname xImplode
#' @export

xImplode... <- function (str, ...) {
	xImplode(str, list(...))
}
