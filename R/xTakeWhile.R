
#' xTakeWhile
#'
#' Take every element in a collection from the start
#' until a predicate returns false.
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
#'    returns the empty list if \code{coll} is
#'    length-zero or the first element of
#'    \code{coll} returns false for the predicate.
#'    Na values are considered false.
#'
#' @family
#'    higher_order_functions
#'
#' @family
#'    collection_functions
#'
#' @export

xTakeWhile <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# take every element until pred returns false

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parameter_missing(pred))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))




	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(pred))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	pred <- match.fun(pred)

	if (length(coll) == 0) {
		list()
	} else {
		for (ith in seq_along(coll)) {

			is_match <- try_higher_order(
				pred( coll[[ith]] ),
				invoking_call)

			assert(
				is.logical(is_match), invoking_call)

			if (!isTRUE(is_match)) {
				return ( as.list(head(coll, ith - 1)) )
			}
		}
		coll
	}
}

#' @export

xTakeWhile... <- function (pred, ...) {
	xTakeWhile(pred, list(...))
}
