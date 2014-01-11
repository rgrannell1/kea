
#' xLocater
#'
#' Get the position of the last element for which a predicate returns true.
#'
#' @param
#'    pred a predicate function.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An length-one or length-zero whole number.
#'
#' @section Corner Cases:
#'      returns integer(0) if no match is found.
#'
#' @template
#'    Variadic
#'
#' @rdname xLocater
#' @export

xLocater <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the last index of collection that matches
	# the predicate.

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
		integer(0)
	} else {

		for (ith in length(coll):1) {

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

#' @rdname xLocater
#' @export

xLocater... <- function (pred, ...) {
	xLocater(pred, list(...))
}
