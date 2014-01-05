
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
#'    a length-one character vector.
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

	assert(
		is_collection(str), invoking_call,
		exclaim$must_be_collection(
			str, summate(str)) )

	str <- as_typed_vector(
		str, 'character', True)

	assert(
		is_collection(strs), invoking_call,
		exclaim$must_be_collection(
			strs, summate(strs)) )

	strs <- as_typed_vector(strs, 'character')

	assert(
		length(str) %in% 0:1,
		exclaim$must_have_length(str, 0:1) )

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
