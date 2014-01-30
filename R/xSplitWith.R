
#' xSplitWith
#'
#' Split a list into two lists; all the elements before the first time a predicate
#' returns false, and all the elements including and after that point.
#'
#' @param
#'      pred a predicate.
#'
#' @param
#'      coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'      A list of two lists.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{coll} is length-zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSplitWith.R
#'
#' @rdname xSplitWith
#' @export

xSplitWith <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [[any] [any]]
	# split a collection before and after a predicate returns true.

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
		list()
	} else {

		coll <- as.list(coll)

		for (ith in seq_along(coll)) {

			is_match <- try_hof(
				pred( coll[[ith]] ),
				invoking_call)

			insist $ must_be_logical_result(is_match, pred, invoking_call)

			if (isTRUE(is_match)) {
				return (
					list(
						head(coll, ith),
						tail(coll, length(coll) - ith) ))
			}
		}
		list(coll, list())
	}
}

#' @rdname xSplitWith
#' @export

xSplitWith... <- function (pred, ...) {
	xSplitWith(pred, list(...))
}
