
#' xFmap
#'
#' Partially apply xMap with a function.
#'
#' @param fn a unary function.
#'
#' @return a unary function of coll.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @family higher_order_functions map_like_functions
#'
#' @example inst/examples/blank.R
#' @export

xFmap <- function (fn) {
	# shorthand for partially applying map.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), pcall,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	function (coll) {
		xMap(fn, coll)
	}
}
