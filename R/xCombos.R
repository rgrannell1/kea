
#' xCombos
#'
#' Generate all ways of choosing several elements from a collection.
#'
#' @param
#'    num a nonnegative whole number.
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of lists, with each list containing \code{num} elements.
#'
#' @section Corner Cases:
#'      Returns the empty list if \code{num} is zero.
#'
#' @family combinatoric_functions
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xCombos
#' @export

xCombos <- function (num, coll) {
	# number -> Collection any
	# generate all ways of choosing several
	# elements from a collection.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(num, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	num <- as_typed_vector(num, 'numeric', True)

	insist$must_be_of_length(num, 1)
	insist$must_be_whole(num, invoking_call)
	insist$must_be_greater_than(num, 0, invoking_call)

	if (num == 0) {
		list()
	} else if (length(coll) == 0) {
		list()
	} else {

		if (is.pairlist(coll)) {
			coll <- as.list(coll)
		}

		num <- min(length(coll), num)
		apply(combn(coll, num), 2, as.list)
	}
}

#' @rdname xCombos
#' @export

xCombos... <- function (num, ...) {
	xCombos(num, list(...))
}
