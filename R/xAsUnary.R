
#' xAsUnary
#'
#' Create a function that takes one argument, and applies each element of
#' that argument as a parameter of its underlying function.
#'
#' @param  fn an arbitrary function.
#'
#' @return a unary function of x.
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xAsUnary <- function (fn) {
	# (... -> b) -> (a -> b)
	# dual to xAsVariadic.
	# takes a function that takes a many values and
	# makes it into a function that takes one list.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)
	remove(invoking_call)

	function (x) {
		xApply(fn, as.list(x))
	}
}
