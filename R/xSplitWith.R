
#' xSplitWith
#' 
#' Split a list into two lists; all the elements before the first time a predicate
#' returns false, and all the elements including and after that point.
#'
#' @param pred a predicate.
#' @param coll a collection.
#'
#' @return a list of two lists.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xSplitWith <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [[any] [any]]
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
		coll <- as.list(coll)
		for (ith in seq_along(coll)) {

			is_match <- pred( coll[[ith]] )
			assert(is.logical(is_match), pcall)

			if (!is_match) {
				return ( 
					list(
						head(coll, ith - 1),
						tail(coll, length(coll) - (ith - 1)) ))
			}
		}
		list(coll, list())
	}
}
