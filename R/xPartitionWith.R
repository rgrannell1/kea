
#' xPartitionWith
#'
#' Divide elements in a collection into two collections based on a predicate function.
#'
#' @param pred a predicate.
#' @param coll a collection.
#'
#' @return Returns two lists; a list of elements in a collection for which a
#' predicate returns true, and a list of elements in a collection for which
#' a predicate returns false or na.
#'
#' @section Corner Cases:
#'	 if \code{coll} is empty the empty list is returned. If all the
#'	 elements return only true/only false, then one of two sublists will be the
#'	 empty list.
#'
#'
#'
#'
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xPartitionWith <- function (pred, coll) {
	# (any -> logical) -> Collection any -> [[any],[any]]
	# returns two lists; a list for which pred returns
	# true, and a list for which pred returns false

	parent_call <- sys.call()

	assert(
		!missing(pred), parent_call,
		exclaim$parameter_missing(pred))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	pred <- dearrowise(pred)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(pred), parent_call,
		exclaim$must_be_matchable(pred))

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	pred <- match.fun(pred)

	if (length(coll) == 0) {
		list()
	} else {
		ind <- vapply(coll, pred, logical(1), USE.NAMES = False)
		true_ind <- !is.na(ind) & ind

		list(
			as.list(coll[true_ind]),
			as.list(coll[!true_ind]) )
	}
}

xPartitionWith... <- function (pred, ...) {
	xPartitionWith(pred, list(...))
}
