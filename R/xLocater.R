
#' xLocater
#'
#' Get the position of the last element for which a predicate returns true.
#'
#' @param
#'    pred a predicate function. The function to test each element
#'    of a collection with.
#'
#' @param
#'    coll a collection. The collection with elements to test.
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
#' @example
#'    inst/examples/example-xLocater.R
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

	insist $ must_be_fn_matchable(pred, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		integer(0)
	} else {

		for (ith in length(coll):1) {

			is_match <- try_hof(
				pred( coll[[ith]] ),
				invoking_call)

			insist $ must_be_logical_result(is_match, pred, invoking_call)

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
