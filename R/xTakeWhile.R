
#' xTakeWhile
#'
#' Take every element in a collection from the start until a predicate returns false.
#'
#' @param pred a predicate.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero or the first element of
#'     \code{coll} returns false for the predicate. Na values are considered false.
#' @template glossary
#'
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xTakeWhile <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# take every element until pred returns false

	pcall <- sys.call()

	assert(
		!missing(pred), pcall,
		exclaim$parameter_missing(pred))

	assert(
		!missing(coll), pcall,
		exclaim$parameter_missing(coll))

	pred <- dearrowise(pred)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(pred), pcall,
		exclaim$must_be_matchable(pred))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	pred <- match.fun(pred)

	if (length(coll) == 0) {
		list()
	} else {
		for (ith in seq_along(coll)) {

			is_match <- pred( coll[[ith]] )

			stopifnot(is.logical(is_match))

			if (!isTRUE(is_match)) {
				return ( as.list(head(coll, ith - 1)) )
			}
		}
		coll
	}
}
