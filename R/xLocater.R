
#' xLocater
#'
#' Get the position of the last element for which a predicate returns true.
#'
#' @param pred a predicate function.
#' @param coll a collection.
#'
#' @return an integer.
#'
#' @section Corner Cases:
#'     returns integer(0) if no match is found.
#' @template glossary
#'
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xLocater <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the last index of collection that matches
	# the predicate.

	parent_call <- sys.call()

	assert(
		!missing(pred), parent_call,
		exclaim$parameter_missing(pred))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	assert(
		is_fn_matchable(pred), parent_call,
		exclaim$must_be_matchable(pred))

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		integer(0)
	} else {

		for (ith in length(coll):1) {
			is_match <- pred( coll[[ith]] )

			assert(is.logical(is_match), parent_call)

			if (is_match) {
				return (as.integer(ith))
			}
		}
		integer(0)
	}
}
