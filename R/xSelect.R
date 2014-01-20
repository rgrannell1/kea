
#' xSelect
#'
#' Include all elements from a collection matching a predicate.
#'
#' @details
#'    \code{xSelect} applies a predicate function to each element
#'    of the input collection, and returns the elements of that
#'    collection such that the predicate returned true.
#'
#' @param
#'    pred a predicate.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll} is length-zero,
#'    or no match is found. If the predicate returns a
#'    non-logical value an error is thrown. If an na value
#'    is returned by the predicate it is treated as a false value.
#'
#' @family filtering_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xSelect
#' @export

xSelect <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# returns coll[i] such that
	# pred(coll[i]) is true

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parametre_missing(pred))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(
			pred, summate(pred)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ind <- try_higher_order(
			vapply(coll, pred, logical(1), USE.NAMES = False),
			invoking_call)

		as.list( coll[ !is.na(ind) & ind ] )
	}
}

#' @rdname xSelect
#' @export

xSelect... <- function (pred, ...) {
	xSelect(pred, list(...))
}
