
#' xLocate
#'
#' Get the position of the first element for which a predicate returns true.
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
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xLocate <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> integer
	# returns the first index of collection that matches
	# the predicate pred.

	pcall <- sys.call()

	#assert(
	#	!missing(pred), pcall,
	#	exclaim$parameter_missing(pred))

	#assert(
	#	!missing(coll), pcall,
	#	exclaim$parameter_missing(coll))

	#assert(
	#	is_fn_matchable(pred), pcall,
	#	exclaim$must_be_matchable(pred))

	#assert(
	#	is_collection(coll), pcall,
	#	exclaim$must_be_collection(coll))

	pred <- match.fun(pred)

	if (length(coll) == 0) {
		integer(0)
	} else {

		for (ith in seq_along(coll)) {


			#assert(
			#	is.logical(is_match), pcall)

			if (is_match) {
				return (as.integer(ith))
			}
		}
		integer(0)
	}
}

#' @export

xLocatel <- xLocate
