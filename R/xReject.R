
#' xReject
#'
#' Remove all elements from a collection matching a predicate.
#'
#' @details
#'    \bold{xReject} applies a predicate function
#'    such as \bold{is.integer} or \bold{is.null} to each element
#'    of a collection. The elements for which the predicate
#'    returned false are kept, and the remaining elements are
#'    removed from the collection.
#'
#' @param
#'    pred a predicate.
#'
#' @param
#'    coll a collection

#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero,
#'    or no match is found. If the predicate returns a
#'    non-logical value an error is thrown. If an na value
#'    is returned by the predicate it is treated as a false value.
#'
#' @family filtering_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xReject
#' @export

xReject <- function (pred, coll) {
	# (a -> boolean) -> Collection a -> [a]
	# returns collection[i] such that
	# pred(collection[i]) is false

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parametre_missing(pred))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_fn_matchable(pred, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ind <- try_hof(
			vapply(coll, pred, logical(1), USE.NAMES = False),
			invoking_call)

		as.list( coll[is.na(ind) | !ind ] )
	}
}

#' @rdname xReject
#' @export

xReject... <- function (pred, ...) {
	xReject(pred, list(...))
}
