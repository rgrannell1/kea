
#' xAsCharacter
#'
#' Convert a collection to a double vector.
#'
#' @param
#'    strs a collection of strings.
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

	assert(
		is_collection(strs), invoking_call,
		exclaim$must_be_collection(
			strs, summate(strs)) )

	as_typed_vector(strs, 'character')
}

#' @rdname xAsCharacter
#' @export

xAsCharacter... <- function (...) {
	xAsCharacter(list(...))
}