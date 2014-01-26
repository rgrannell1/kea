
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
#'    inst/examples/example-xLocatel.R
#'
#' @rdname xLocate
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

	insist$must_be_fn_matchable(pred, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		integer(0)
	} else {

		for (ith in seq_along(coll)) {

			is_match <- try_hof(
				pred( coll[[ith]] ), invoking_call)

			insist$is_logical_result(is_match, pred, invoking_call)

			if (isTRUE(is_match)) {
				return (as.integer(ith))
			}
		}
		integer(0)
	}
}

#' @rdname xLocate
#' @export

xLocatel <- xLocate

#' @rdname xLocate
#' @export

xLocatel... <- function (pred, ...) {
	xLocatel(pred, list(...))
}

#' @rdname xLocate
#' @export

xLocate... <- xLocatel...
