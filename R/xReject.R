
#' xReject
#'
#' Remove all elements from a collection matching a predicate.
#'
#' @section Uses:
#'    The uses for the reject function are very similar to
#'    those for select.
#'
#' @param
#'    pred a predicate.
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero.
#'    Throws an error if pred returns a non-boolean value.
#'
#' @family higher_order_functions
#'
#' @family collection_functions
#'
#' @family filtering_functions
#'
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

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(
			pred, profile_object(pred)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	pred <- match.fun(pred)

	if (length(coll) == 0) {
		list()
	} else {

		ind <- try_higher_order(
			vapply(coll, pred, logical(1), USE.NAMES = False),
			invoking_call)

		as.list( coll[is.na(ind) | !ind ] )
	}
}

#' @export

xReject... <- function (pred, ...) {
	xReject(pred, list(...))
}
