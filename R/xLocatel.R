
#' xLocate
#'
#' Get the position of the first element for which a
#' predicate returns true.
#'
#' @param
#'    pred a predicate function.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    an length-one or length-zero whole number.
#'
#' @section Corner Cases:
#'     returns integer(0) if no match is found.
#'
#' @family higher_order_functions
#'
#' @family collection_functions
#'
#' @export

xLocate <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the first index of collection that matches
	# the predicate pred.

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
		integer(0)
	} else {

		for (ith in seq_along(coll)) {

			is_match <- try_higher_order(
				pred( coll[[ith]] ),
				invoking_call)

			if (isTRUE(is_match)) {
				return (as.integer(ith))
			}
		}
		integer(0)
	}
}

#' @export

xLocatel <- xLocate

#' @export

xLocatel... <- function (pred, ...) {
	xLocatel(pred, list(...))
}

#' @export

xLocate... <- xLocatel...
