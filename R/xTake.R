
#' xTake
#'
#' Take several elements from the head of a collection.
#'
#' @param
#'     num a nonnegative whole number.
#'
#' @param
#'    ... see above.
#'
#' @param
#'     coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'    If \code{coll} is empty the empty list is returned.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xTake
#' @export

xTake <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) == 1, invoking_call,
		exclaim$must_have_length(num, 1))

	assert(
		num >= 0, invoking_call,
		exclaim$must_be_greater_than(
			num, 0, summate(num)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	if (length(coll) == 0 || num == 0) {
		list()
	} else {
		as.list(coll)[seq_len( min(num, length(coll)) )]
	}
}

#' @rdname xTake
#' @export

xTake... <- function (num, ...) {
	xTake(num, list(...))
}
