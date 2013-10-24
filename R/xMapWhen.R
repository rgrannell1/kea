
#' xMapWhen
#'
#' Selectively apply a function to elements in a collection.
#' 
#' @param pred a predicate function.
#' @param fn a unary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#'
#' @family higher_order_functions map_like_functions
#'
#' @example inst/examples/blank.R
#' @export

xMapWhen <- function (pred, fn, coll) {
	# (any -> boolean) -> (any -> any) -> Collection any -> [any]
	# apply the function pred to collection, and apply f to
	# the elements for which pred returned true.

	pcall <- sys.call()

	assert(
		!missing(pred), pcall,
		exclaim$parameter_missing(pred))
	
	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	pred <- dearrowise(pred)
	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(pred), pcall,
		exclaim$must_be_matchable(pred))

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))
			
	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	pred <- match.fun(pred)
	fn <- match.fun(fn)

	composite <- function (x) {
		is_match <- pred(x)
		assert(is.logical(is_match), pcall)

		if (is_match) fn(x) else x
	}

	if (length(coll) == 0) {
		list()
	} else {
		xMap(composite, coll)
	}
}
