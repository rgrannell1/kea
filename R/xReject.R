
#' xReject
#'
#' Remove all elements from a collection matching a predicate.
#'
#' @section Uses:
#'     The uses for the reject function are very similar to
#'     those for select.
#'
#' @param pred a predicate.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'     Throws an error if pred returns a non-boolean value.
#' @template glossary
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xReject <- function (pred, coll) {
	# (a -> boolean) -> Collection a -> [a]
	# returns collection[i] such that
	# pred(collection[i]) is false

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
		ind <- vapply(coll, pred, logical(1), USE.NAMES = False)
		as.list( coll[is.na(ind) | !ind ] )
	}
}
