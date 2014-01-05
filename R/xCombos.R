
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
#'    a list of lists, with each list containing \code{num} elements.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{num} is zero.
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

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) %in% 0:1, invoking_call,
		exclaim$must_have_length(
			num, 0:1, summate(num)) )

	assert(
		num >= 0, invoking_call,
		exclaim$must_be_greater_than(
			num, 0, summate(num)) )

	assert(
		round(num) == num, invoking_call,
		exclaim$must_be_whole(
			num, summate(num)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

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
