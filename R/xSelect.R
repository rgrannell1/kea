
#' xSelect
#'
#' Include all elements from a collection matching a predicate.
#'
#' @section Uses:
#' The select function is useful for taking a collection,
#' and returning values that meet certain criteria. Likely
#' uses include selecting rows in a (converted) data frame
#' that contain a certain value, selecting strings that
#' match a regular expression, or selecting records
#' based on a value in a particular field.
#'
#' @param pred a predicate.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero, or no match is found.
#' @template glossary
#'
#' @family higher_order_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xSelect <- function (pred, coll) {
	# (any -> boolean) -> Collection any -> [any]
	# returns coll[i] such that
	# pred(coll[i]) is true

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

	pred <- match_fn(pred)

	if (length(coll) == 0) {
		list()
	} else {
		ind <- vapply(coll, pred, logical(1), USE.NAMES = False)
		as.list( coll[ !is.na(ind) & ind ] )
	}
}

#' @export

xFilter <- xSelect
