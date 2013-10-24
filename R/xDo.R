#' xDo
#' 
#' Map (a possibly side-effectful) function over a collection and discard the results.
#'
#' @param fn a unary function, usually side-effectful.
#' @param coll a collection
#'
#' @return a list.
#'
#' @template glossary
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xDo <- function (fn, coll) {
	# function -> Collection any -> NULL
	# apply a function to each element of a collection.
	# and discard the results.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))
	
	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)
	
	if (length(coll) == 0) {
		list()
	} else {
		for (ith in seq_along(coll)) {
			fn( coll[[ith]] )
		}
		invisible (NULL)
	}
}
