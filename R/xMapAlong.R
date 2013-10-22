
#' xMapAlong
#' 
#' Apply a binary function to each element of a collection and its indices.
#'
#' @param fn a binary function.
#' @param coll a collection.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list is \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @family higher_order_function
#'
#' @example inst/examples/blank.R
#' @export

xMapAlong <- function (fn, coll) {
	# (integer -> any -> any) -> Collection any -> [any]

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
		Map(
			function (ind) {
				fn( coll[[ind]], ind )
			},
			seq_along(coll)
		)
	}
}
